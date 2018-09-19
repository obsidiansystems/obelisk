{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Run where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Either
import Data.List
import Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult (runParseResult)
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic (withUTF8FileContents, toUTF8BS)
import Language.Haskell.Extension
import Network.Socket
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Data.ByteString (ByteString)

import Obelisk.App (MonadObelisk, ObeliskT)
import Obelisk.CliApp (CliT (..), HasCliConfig, Severity (..), callCommand, failWith, getCliConfig, putLog,
                       runCli)
import Obelisk.Command.Project (inProjectShell)

data CabalPackageInfo = CabalPackageInfo
  { _cabalPackageInfo_packageRoot :: FilePath
  , _cabalPackageInfo_sourceDirs :: NE.NonEmpty FilePath
    -- ^ List of hs src dirs of the library component
  , _cabalPackageInfo_defaultExtensions :: [Extension]
    -- ^ List of globally enable extensions of the library component
  }

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: ObeliskT IO ()
run = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    freePort <- getFreePort
    runGhcid dotGhciPath $ Just $ unwords ["run", show freePort, "Backend.backend", "Frontend.frontend"]

runRepl :: MonadObelisk m => m ()
runRepl = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    runGhciRepl dotGhciPath

runWatch :: MonadObelisk m => m ()
runWatch = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> runGhcid dotGhciPath Nothing

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: Applicative f => f [FilePath]
getLocalPkgs = pure ["backend", "common", "frontend"]

parseCabalPackage
  :: (MonadObelisk m)
  => FilePath -- ^ package cabal file path
  -> m (Maybe CabalPackageInfo)
parseCabalPackage cabalFp = do
  exists <- liftIO $ doesFileExist cabalFp
  if exists
    then do
      withUTF8FileContentsM cabalFp $ \cabal -> do
        let (warnings, result) = runParseResult $ parseGenericPackageDescription cabal
        mapM_ (putLog Warning) $ fmap (T.pack . show) warnings
        case result of
          Right gpkg -> do
            return $ do
              (_, lib) <- simplifyCondTree (const $ pure True) <$> condLibrary gpkg
              pure $ CabalPackageInfo
                { _cabalPackageInfo_packageRoot = takeDirectory cabalFp
                , _cabalPackageInfo_sourceDirs =
                    fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
                , _cabalPackageInfo_defaultExtensions =
                    defaultExtensions $ libBuildInfo lib
                }
          Left (_, errors) -> do
            putLog Error $ T.pack "Failed to parse cabal file: "
            mapM_ (putLog Error) $ fmap (T.pack . show) errors
            return Nothing
    else return Nothing

withUTF8FileContentsM :: (MonadIO m, HasCliConfig m) => FilePath -> (ByteString -> CliT IO a) -> m a
withUTF8FileContentsM fp f = do
  c <- getCliConfig
  liftIO $ withUTF8FileContents fp $ runCli c . f . toUTF8BS

-- | Create ghci configuration to load the given packages
withGhciScript
  :: MonadObelisk m
  => [FilePath] -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporory .ghci
  -> m ()
withGhciScript pkgs f = do
  (pkgDirErrs, packageInfos) <- fmap partitionEithers $ forM pkgs $ \pkg -> do
    let cabalFp = pkg </> pkg <.> "cabal"
    flip fmap (parseCabalPackage cabalFp) $ \case
      Nothing -> Left pkg
      Just packageInfo -> Right packageInfo

  when (null packageInfos) $
    failWith $ T.pack $ "No valid pkgs found in " <> intercalate ", " pkgs
  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs

  let extensions = packageInfos >>= _cabalPackageInfo_defaultExtensions
      extensionsLine = if extensions == mempty
        then ""
        else ":set " <> intercalate " " ((("-X" <>) . prettyShow) <$> extensions)
      dotGhci = unlines $
        [ ":set -i" <> intercalate ":" (packageInfos >>= rootedSourceDirs)
        , extensionsLine
        , ":load Backend Frontend"
        , "import Obelisk.Run"
        , "import qualified Frontend"
        , "import qualified Backend"
        ]
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath

  where
    rootedSourceDirs pkg = NE.toList $
      (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg

-- | Run ghci repl
runGhciRepl
  :: MonadObelisk m
  => FilePath -- ^ Path to .ghci
  -> m ()
runGhciRepl dotGhci = inProjectShell "ghc" $ unwords $ "ghci" : ["-ghci-script", dotGhci]

-- | Run ghcid
runGhcid
  :: MonadObelisk m
  => FilePath -- ^ Path to .ghci
  -> Maybe String -- ^ Optional command to run at every reload
  -> m ()
runGhcid dotGhci mcmd = callCommand $ unwords $ "ghcid" : opts
  where
    opts =
      [ "-W"
      --TODO: The decision of whether to use -fwarn-redundant-constraints should probably be made by the user
      , "--command='ghci -Wall -fwarn-redundant-constraints -ghci-script " <> dotGhci <> "' "
      , "--reload=config"
      , "--outputfile=ghcid-output.txt"
      ] <> testCmd
    testCmd = maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'")

getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock

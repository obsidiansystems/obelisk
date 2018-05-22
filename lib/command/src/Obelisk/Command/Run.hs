{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.Command.Run where

import qualified Control.Exception as E
import Control.Monad
import Data.Either
import Data.List
import Control.Monad.Reader (ask)
import Data.Maybe
import Control.Monad.IO.Class (liftIO)
import Data.Monoid
import qualified Data.Text as T
import Data.List.NonEmpty as NE
import Distribution.Utils.Generic (withUTF8FileContents)
import Distribution.PackageDescription.Parse (parseGenericPackageDescription, ParseResult(..))
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.Library
import Distribution.Types.GenericPackageDescription
import Network.Socket
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)

import Obelisk.App (MonadObelisk, ObeliskT, runObelisk)
import Obelisk.CLI (callCommand, putLog, failWith, Severity(..))
import Obelisk.Command.Project (inProjectShell)

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: ObeliskT IO ()
run = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    freePort <- getFreePort
    let testCmd = unwords ["Obelisk.Run.run", show freePort, "backend", "frontend"]
    runGhcid dotGhciPath $ Just testCmd

runRepl :: MonadObelisk m => m ()
runRepl = do
  pkgs <- getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    runGhciRepl dotGhciPath

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: MonadObelisk m => m [FilePath]
getLocalPkgs = return ["backend", "common", "frontend"]

parseHsSrcDir
  :: MonadObelisk m
  => FilePath -- ^ package cabal file path
  -> m (Maybe (NE.NonEmpty FilePath)) -- ^ List of hs src dirs of the library component
parseHsSrcDir cabalFp = do
  exists <- liftIO $ doesFileExist cabalFp
  if exists
    then do
      c <- ask
      liftIO $ withUTF8FileContents cabalFp $ \cabal -> runObelisk c $ do
      case parseGenericPackageDescription cabal of
        ParseOk warnings gpkg -> do
          mapM_ (putLog Warning) $ fmap (T.pack . show) warnings
          return $ do
            (_, lib) <- simplifyCondTree (const $ pure True) <$> condLibrary gpkg
            pure $ fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
        ParseFailed e -> do
          putLog Error $ T.pack $ "Failed to parse cabal file: " <> show e
          return Nothing
    else return Nothing

-- | Create ghci configuration to load the given packages
withGhciScript
  :: MonadObelisk m
  => [FilePath] -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporory .ghci
  -> m ()
withGhciScript pkgs f = do
  (pkgDirErrs, hsSrcDirs) <- fmap partitionEithers $ forM pkgs $ \pkg -> do
    let cabalFp = pkg </> pkg <.> "cabal"
    xs <- parseHsSrcDir cabalFp
    return $ case xs of
      Nothing -> Left pkg
      Just hsSrcDirs -> Right $ toList $ fmap (pkg </>) hsSrcDirs
  when (null hsSrcDirs) $
    failWith $ T.pack $ "No valid pkgs found in " <> intercalate ", " pkgs
  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs
  let dotGhci = unlines
        [ ":set -i" <> intercalate ":" (mconcat hsSrcDirs)
        , ":add Backend Frontend"
        , ":module + Obelisk.Run Frontend Backend"
        ]
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath

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
      , "--command='ghci -ghci-script " <> dotGhci <> "' "
      , "--reload=config"
      , "--outputfile=ghcid-output.txt"
      ] <> testCmd
    testCmd = maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'")

getFreePort :: MonadObelisk m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  E.bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock

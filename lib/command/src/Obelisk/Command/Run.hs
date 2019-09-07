{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Run where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult (runParseResult)
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic
import GHC.Generics (Generic)
import Hpack.Config
import Hpack.Render
import Hpack.Yaml
import Language.Haskell.Extension
import Network.Socket hiding (Debug)
import System.Directory
import System.FilePath
import System.Process (proc)
import System.IO.Temp (withSystemTempDirectory)
import Data.ByteString (ByteString)

import Obelisk.App (MonadObelisk, ObeliskT)
import Obelisk.CliApp
  ( CliT (..), HasCliConfig, Severity (..)
  , callCommand, failWith, getCliConfig, putLog
  , readProcessAndLogStderr, runCli)
import Obelisk.Command.Project (inProjectShell, withProjectRoot)

data CabalPackageInfo = CabalPackageInfo
  { _cabalPackageInfo_packageRoot :: FilePath
  , _cabalPackageInfo_sourceDirs :: NE.NonEmpty FilePath
    -- ^ List of hs src dirs of the library component
  , _cabalPackageInfo_defaultExtensions :: [Extension]
    -- ^ List of globally enable extensions of the library component
  , _cabalPackageInfo_defaultLanguage :: Maybe Language
    -- ^ List of globally set languages of the library component
  }

data ReplOpts = ReplOpts
  { _replOpts_import :: [(String, String)]
  -- ^ Modules to load (in addition to Frontend/Backend).
  -- First argument is the library name, second is the module name.
  -- For example, [(some-frontend, Some.Other.Frontend)]
  } deriving (Generic, Show)
instance ToJSON ReplOpts
instance FromJSON ReplOpts

data RunOpts = RunOpts
  { _runOpts_replOpts :: ReplOpts
  -- ^ Repl options
  , _runOpts_frontend :: Maybe (String, String)
  -- ^ Alternative frontend to load.
  -- First argument is the module, second is the function in the module.
  -- The module must be imported (see 'runOptsModulesToLoad').
  -- For example, (Some.Other.Frontend, myFrontend)
  , _runOpts_backend :: Maybe (String, String)
  -- ^ Alternative backend to load.
  -- First argument is the module, second is the function in the module.
  -- The module must be imported (see 'runOptsModulesToLoad').
  -- For example, (Some.Other.Backend, myBackend)
  } deriving (Generic, Show)
instance ToJSON RunOpts
instance FromJSON RunOpts

-- | Decode 'RunOpts' from a base 16 bytestring
decodeRunOpts :: String -> Either String RunOpts
decodeRunOpts = Aeson.eitherDecodeStrict . fst . B16.decode . T.encodeUtf8 . T.pack

-- | Encode 'RunOpts' to a base 16 bytestring. This uses base 16 to avoid
-- shell quoting issues with JSON
encodeRunOpts :: RunOpts -> String
encodeRunOpts = T.unpack . T.decodeUtf8 . B16.encode . LBS.toStrict . Aeson.encode

-- | Get the fully qualified frontend to use.
runOptsFrontend :: MonadObelisk m => RunOpts -> m (Maybe String)
runOptsFrontend opts
  | Just (m, fun) <- _runOpts_frontend opts
  = if any (\(_, m') -> m' == m) (_replOpts_import $ _runOpts_replOpts opts)
    then pure $ Just $ m <> "." <> fun
    else putLog Warning "Ignoring alternate frontend: the module is not imported" >> pure Nothing
  | otherwise = pure Nothing

-- | Get the fully qualified backend to use.
runOptsBackend :: MonadObelisk m => RunOpts -> m (Maybe String)
runOptsBackend opts
  | Just (m, fun) <- _runOpts_backend opts
  = if any (\(_, m') -> m' == m) (_replOpts_import $ _runOpts_replOpts opts)
    then pure $ Just $ m <> "." <> fun
    else putLog Warning "Ignoring alternate backend: the module is not imported" >> pure Nothing
  | otherwise = pure Nothing

runOptsModulesToLoad :: RunOpts -> [String]
runOptsModulesToLoad opts = mconcat
  [ ["Frontend", "Backend"]
  , snd <$> _replOpts_import (_runOpts_replOpts opts)
  ]

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: RunOpts -> ObeliskT IO ()
run opts = do
  pkgs <- getLocalPkgs opts
  withGhciScript opts pkgs $ \dotGhciPath -> do
    freePort <- getFreePort
    assets <- withProjectRoot "." $ \root -> do
      let importableRoot = if "/" `isInfixOf` root
            then root
            else "./" <> root
      isDerivation <- readProcessAndLogStderr Debug $
        proc "nix"
          [ "eval"
          , "-f"
          , root
          , "(let a = import " <> importableRoot <> " {}; in toString (a.reflex.nixpkgs.lib.isDerivation a.passthru.staticFilesImpure))"
          , "--raw"
          -- `--raw` is not available with old nix-instantiate. It drops quotation
          -- marks and trailing newline, so is very convenient for shelling out.
          ]
      -- Check whether the impure static files are a derivation (and so must be built)
      if isDerivation == "1"
        then fmap T.strip $ readProcessAndLogStderr Debug $ -- Strip whitespace here because nix-build has no --raw option
          proc "nix-build"
            [ "--no-out-link"
            , "-E", "(import " <> importableRoot <> "{}).passthru.staticFilesImpure"
            ]
        else readProcessAndLogStderr Debug $
          proc "nix" ["eval", "-f", root, "passthru.staticFilesImpure", "--raw"]
    putLog Debug $ "Assets impurely loaded from: " <> assets
    mFrontend <- runOptsFrontend opts
    mBackend <- runOptsBackend opts
    runGhcid dotGhciPath $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(runServeAsset " ++ show assets ++ ")"
      , fromMaybe "Backend.backend" mBackend
      , fromMaybe "Frontend.frontend" mFrontend
      ]

runRepl :: MonadObelisk m => RunOpts -> m ()
runRepl opts = do
  pkgs <- getLocalPkgs opts
  withGhciScript opts pkgs $ \dotGhciPath -> do
    runGhciRepl dotGhciPath

runWatch :: MonadObelisk m => RunOpts -> m ()
runWatch opts = do
  pkgs <- getLocalPkgs opts
  withGhciScript opts pkgs $ \dotGhciPath -> runGhcid dotGhciPath Nothing

-- | Relative paths to local packages of an obelisk project
-- TODO a way to query this
getLocalPkgs :: Applicative f => RunOpts -> f [FilePath]
getLocalPkgs opts = pure $ mconcat
  [ ["backend", "common", "frontend"]
  , fst <$> _replOpts_import (_runOpts_replOpts opts)
  ]

parseCabalPackage
  :: (MonadObelisk m)
  => FilePath -- ^ package directory
  -> m (Maybe CabalPackageInfo)
parseCabalPackage dir = do
  let cabalFp = dir </> (takeBaseName dir <> ".cabal")
      hpackFp = dir </> "package.yaml"
  hasCabal <- liftIO $ doesFileExist cabalFp
  hasHpack <- liftIO $ doesFileExist hpackFp

  mCabalContents <- if hasCabal
    then Just <$> liftIO (readUTF8File cabalFp)
    else if hasHpack
      then do
        let decodeOptions = DecodeOptions (ProgramName "ob") hpackFp Nothing decodeYaml
        liftIO (readPackageConfig decodeOptions) >>= \case
          Left err -> do
            putLog Error $ T.pack $ "Failed to parse " <> hpackFp <> ": " <> err
            return Nothing
          Right (DecodeResult hpackPackage _ _ _) -> do
            return $ Just $ renderPackage [] hpackPackage
      else return Nothing

  fmap join $ forM mCabalContents $ \cabalContents -> do
    let (warnings, result) = runParseResult $ parseGenericPackageDescription $
          toUTF8BS $ cabalContents
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
            , _cabalPackageInfo_defaultLanguage =
                defaultLanguage $ libBuildInfo lib
            }
      Left (_, errors) -> do
        putLog Error $ T.pack $ "Failed to parse " <> cabalFp <> ":"
        mapM_ (putLog Error) $ fmap (T.pack . show) errors
        return Nothing

withUTF8FileContentsM :: (MonadIO m, HasCliConfig e m) => FilePath -> (ByteString -> CliT e IO a) -> m a
withUTF8FileContentsM fp f = do
  c <- getCliConfig
  liftIO $ withUTF8FileContents fp $ runCli c . f . toUTF8BS

-- | Create ghci configuration to load the given packages
withGhciScript
  :: MonadObelisk m
  => RunOpts
  -> [FilePath] -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporory .ghci
  -> m ()
withGhciScript opts pkgs f = do
  (pkgDirErrs, packageInfos) <- fmap partitionEithers $ forM pkgs $ \pkg -> do
    flip fmap (parseCabalPackage pkg) $ \case
      Nothing -> Left pkg
      Just packageInfo -> Right packageInfo

  when (null packageInfos) $
    failWith $ T.pack $ "No valid pkgs found in " <> intercalate ", " pkgs
  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find pkgs in " <> intercalate ", " pkgDirErrs

  let extensions = packageInfos >>= _cabalPackageInfo_defaultExtensions
      languageFromPkgs = L.nub $ mapMaybe _cabalPackageInfo_defaultLanguage packageInfos
      -- NOTE when no default-language is present cabal sets Haskell98
      language = NE.toList $ fromMaybe (Haskell98 NE.:| []) $ NE.nonEmpty languageFromPkgs
      extensionsLine = if extensions == mempty
        then ""
        else ":set " <> intercalate " " ((("-X" <>) . prettyShow) <$> extensions)
      dotGhci = unlines $
        [ ":set -i" <> intercalate ":" (packageInfos >>= rootedSourceDirs)
        , extensionsLine
        , ":set " <> intercalate " " (("-X" <>) . prettyShow <$> language)
        , ":load " <> intercalate " " (runOptsModulesToLoad opts)
        , "import Obelisk.Run"
        ] ++ fmap ("import qualified " <>) (runOptsModulesToLoad opts)
  warnDifferentLanguages language
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath

  where
    rootedSourceDirs pkg = NE.toList $
      (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg

warnDifferentLanguages :: MonadObelisk m => [Language] -> m ()
warnDifferentLanguages (_:_:_) = putLog Warning "Different languages detected across packages which may result in errors when loading the repl"
warnDifferentLanguages _ = return ()

-- | Run ghci repl
runGhciRepl
  :: MonadObelisk m
  => FilePath -- ^ Path to .ghci
  -> m ()
runGhciRepl dotGhci = inProjectShell "ghc" $ unwords $ "ghci" : ["-no-user-package-db", "-ghci-script", dotGhci]

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
      , "--command='ghci -Wall -ignore-dot-ghci -fwarn-redundant-constraints -no-user-package-db -ghci-script " <> dotGhci <> "' "
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

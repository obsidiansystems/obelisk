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
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Text as T
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult (runParseResult)
import Distribution.Pretty
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic
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
  , readProcessAndLogStderr, readProcessJSONAndLogStderr, runCli)
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

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: ObeliskT IO ()
run = withProjectRoot "." $ \root -> do
  pkgs <- getLocalPkgs root
  withGhciScript pkgs $ \dotGhciPath -> do
    freePort <- getFreePort
    assets <- do
      let importableRoot = toNixPath root
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
    runGhcid dotGhciPath $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(runServeAsset " ++ show assets ++ ")"
      , "Backend.backend"
      , "Frontend.frontend"
      ]

-- | Nix syntax requires relative paths to be prefixed by @./@ or
-- @../@. This will make a 'FilePath' that can be embedded in a Nix
-- expression.
toNixPath :: FilePath -> FilePath
toNixPath root | "/" `isInfixOf` root = root
               | otherwise = "./" <> root

runRepl :: MonadObelisk m => m ()
runRepl = do
  pkgs <- withProjectRoot "." getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> do
    runGhciRepl dotGhciPath

runWatch :: MonadObelisk m => m ()
runWatch = do
  pkgs <- withProjectRoot "." getLocalPkgs
  withGhciScript pkgs $ \dotGhciPath -> runGhcid dotGhciPath Nothing

-- | Relative paths to local packages of an obelisk project.
--
-- These are a combination of the obelisk predefined local packages,
-- and any packages that the user has set with the @packages@ argument
-- to the Nix @project@ function.
getLocalPkgs :: (MonadObelisk m, MonadIO m) => FilePath -> m [FilePath]
getLocalPkgs root = do
  root' <- liftIO $ makeAbsolute root
  -- package paths from nix project will be absolute paths
  projectPackages <- readProcessJSONAndLogStderr Debug $
    proc "nix"
      [ "eval"
      , "(let proj = import " <> root <> " {}; in (map toString (proj.obelisk.reflex-platform.nixpkgs.lib.attrValues (proj.passthru.packages or {}))))"
      , "--json"
      ]
  pure $ predefinedLocalPkgs ++ map (makeRelative root') projectPackages

-- | Relative paths to the predefined obelisk project packages.
-- (See @predefinedPackages@ in @default.nix@)
predefinedLocalPkgs :: [FilePath]
predefinedLocalPkgs = ["backend", "common", "frontend"]

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
  => [FilePath] -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporory .ghci
  -> m ()
withGhciScript pkgs f = do
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
        , ":load Backend Frontend"
        , "import Obelisk.Run"
        , "import qualified Frontend"
        , "import qualified Backend"
        ]
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

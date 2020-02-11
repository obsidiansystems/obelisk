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
import Data.Time (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Distribution.Compiler (CompilerFlavor(..))
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
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.FilePath
import Data.ByteString (ByteString)
import System.IO (hPutStr, hFlush, hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (proc, waitForProcess)

import Obelisk.App (MonadObelisk, ObeliskT)
import Obelisk.CliApp
  ( CliT (..), HasCliConfig, Severity (..)
  , callCommand, failWith, getCliConfig, putLog
  , readProcessAndLogStderr, runCli
  , readProcessAndLogStderr, createProcess_)
import Obelisk.Command.Project (inProjectShell, withProjectRoot, setCwd, setCtlc)

data CabalPackageInfo = CabalPackageInfo
  { _cabalPackageInfo_packageRoot :: FilePath
  , _cabalPackageInfo_sourceDirs :: NE.NonEmpty FilePath
    -- ^ List of hs src dirs of the library component
  , _cabalPackageInfo_defaultExtensions :: [Extension]
    -- ^ List of globally enable extensions of the library component
  , _cabalPackageInfo_defaultLanguage :: Maybe Language
    -- ^ List of globally set languages of the library component
  , _cabalPackageInfo_compilerOptions :: [(CompilerFlavor, [String])]
    -- ^ List of compiler-specific options (e.g., the "ghc-options" field of the cabal file)
  }

-- NOTE: `run` is not polymorphic like the rest because we use StaticPtr to invoke it.
run :: Bool -> ObeliskT IO ()
run profiled = withProjectRoot "." $ \root -> do
  pkgs <- getLocalPkgs
  freePort <- getFreePort
  assets <- do
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
  let obRunExpr = unwords
          [ "Obelisk.Run.run"
          , show freePort
          , "(Obelisk.Run.runServeAsset " ++ show assets ++ ")"
          , "Backend.backend"
          , "Frontend.frontend"
          ]
  putLog Debug $ "Assets impurely loaded from: " <> assets
  case profiled of
    True -> do
      putLog Debug "Using profiled build of project."
      time <- liftIO $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime
      let exeSource =
            unlines $
              [ "module Main where"
              , "import Control.Exception"
              , "import Reflex.Profiled" ]
              <> obRunImports <>
              [ "main :: IO ()"
              , "main = " <> obRunExpr <> " `finally` writeProfilingData \"" <> profileBaseName <> ".rprof\"" ]
          -- Sane flags to enable by default, enable time profiling +
          -- closure heap profiling.
          rtsFlags = [ "+RTS", "-p", "-po" <> profileBaseName, "-hc", "-RTS" ]
          profileDirectory = root </> "profile"
          profileBaseName = profileDirectory </> time
      liftIO $ createDirectoryIfMissing False profileDirectory
      withSystemTempFile "ob-run-profiled.hs" $ \hsFname hsHandle -> withSystemTempFile "ob-run" $ \exeFname exeHandle -> do
        liftIO $ hPutStr hsHandle exeSource
        liftIO $ hFlush hsHandle
        (_, _, _, ph1) <- createProcess_ "nixGhcWithProfiling" $ setCwd (Just root) $ proc "nix-shell" [ "-p", "((import ./. {}).profiled.ghc.ghcWithPackages (p: [ p.backend p.frontend]))", "--run", unwords [ "ghc", "-x", "hs", "-prof", "-fno-prof-auto", hsFname, "-o", exeFname ] ]
        code <- liftIO $ waitForProcess ph1
        case code of
          ExitSuccess -> do
            liftIO $ hClose exeHandle
            (_, _, _, ph2) <- createProcess_ "runProfExe" $ setCwd (Just root) $ setCtlc $ proc exeFname rtsFlags
            _ <- liftIO $ waitForProcess ph2
            pure ()
          ExitFailure _ -> do
            pure ()
    False ->
      withGhciScript pkgs $ \dotGhciPath -> do
        runGhcid dotGhciPath $ Just obRunExpr

-- | Used to signal to obelisk that it's being invoked as a preprocessor
preprocessorIdentifier :: String
preprocessorIdentifier = "__preprocessor-apply-packages"

-- | Imports needed to use Obelisk.Run
obRunImports :: [String]
obRunImports = [ "import qualified Obelisk.Run"
               , "import qualified Frontend"
               , "import qualified Backend" ]

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
            , _cabalPackageInfo_compilerOptions = options $ libBuildInfo lib
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
      ghcOptions = concat $ mapMaybe (\case (GHC, xs) -> Just xs; _ -> Nothing) $
        packageInfos >>= _cabalPackageInfo_compilerOptions
      dotGhci = unlines $
        [ ":set -i" <> intercalate ":" (packageInfos >>= rootedSourceDirs)
        , case ghcOptions of
            [] -> ""
            xs -> ":set " <> intercalate " " xs
        , extensionsLine
        , ":set " <> intercalate " " (("-X" <>) . prettyShow <$> language)
        , ":load Backend Frontend"
        ] <> obRunImports
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

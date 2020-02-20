{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}
module Obelisk.Command.Run where

import Control.Arrow ((&&&))
import Control.Exception (Exception, bracket)
import Control.Lens (ifor, (.~), (&))
import Control.Monad (filterM, unless, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Either
import Data.Foldable (for_, toList)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.String.Here (hereLit)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Traversable (for)
import Debug.Trace (trace)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.Parsec.ParseResult (runParseResult)
import qualified Distribution.System as Dist
import Distribution.Types.BuildInfo
import Distribution.Types.CondTree
import Distribution.Types.GenericPackageDescription
import Distribution.Types.Library
import Distribution.Utils.Generic
import qualified Distribution.Parsec.Common as Dist
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import qualified Hpack.Yaml as Hpack
import Language.Haskell.Extension
import Network.Socket hiding (Debug)
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import qualified System.Info
import System.IO.Temp (withSystemTempDirectory)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..),
                       failWith, putLog, proc, readCreateProcessWithExitCode,
                       setCwd, setDelegateCtlc, createProcess_, waitForProcess)
import Obelisk.Command.Nix
import Obelisk.Command.Project (nixShellWithPkgs, toImplDir, withProjectRoot, findProjectAssets)
import Obelisk.Command.Thunk (attrCacheFileName)
import Obelisk.Command.Utils (findExePath, ghcidExePath)

data CabalPackageInfo = CabalPackageInfo
  { _cabalPackageInfo_packageFile :: FilePath
  , _cabalPackageInfo_packageName :: T.Text
  , _cabalPackageInfo_packageRoot :: FilePath
  , _cabalPackageInfo_buildable :: Bool
  , _cabalPackageInfo_sourceDirs :: NE.NonEmpty FilePath
    -- ^ List of hs src dirs of the library component
  , _cabalPackageInfo_defaultExtensions :: [Extension]
    -- ^ List of globally enable extensions of the library component
  , _cabalPackageInfo_defaultLanguage :: Maybe Language
    -- ^ List of globally set languages of the library component
  , _cabalPackageInfo_compilerOptions :: [(CompilerFlavor, [String])]
    -- ^ List of compiler-specific options (e.g., the "ghc-options" field of the cabal file)
  }

-- | Used to signal to obelisk that it's being invoked as a preprocessor
preprocessorIdentifier :: String
preprocessorIdentifier = "__preprocessor-apply-packages"

profile
  :: MonadObelisk m
  => String
  -> [String]
  -> m ()
profile profileBasePattern rtsFlags = withProjectRoot "." $ \root -> do
  putLog Debug "Using profiled build of project."

  time <- liftIO getCurrentTime
  let profileBaseName = formatTime defaultTimeLocale profileBasePattern time

  putLog Debug $ T.pack $ "Storing profiled data under base name of " <> profileBaseName

  liftIO $ createDirectoryIfMissing False $ takeDirectory profileBaseName

  exePath <- nixCmd $ NixCmd_Build $ def
      & nixBuildConfig_outLink .~ OutLink_None
      & nixCmdConfig_target .~ Target
        { _target_path = Nothing
        , _target_attr = Nothing
        , _target_expr = Just nixBuildExpr }
  assets <- findProjectAssets root
  putLog Debug $ "Assets impurely loaded from: " <> assets
  freePort <- getFreePort
  (_, _, _, ph) <- createProcess_ "runProfExe" $ setCwd (Just root) $ setDelegateCtlc True $ proc exePath $
    [ show freePort
    , T.unpack assets
    , profileBaseName
    , "+RTS"
    , "-po" <> profileBaseName
    ] <> rtsFlags
      <> [ "-RTS" ]
  void $ waitForProcess ph
  where
    nixBuildExpr = [hereLit|
with (import ./. {});
let
  exeSource = obelisk.nixpkgs.writeText "ob-run" ''
module Main where

import Control.Exception
import Reflex.Profiled
import System.Environment

import qualified Obelisk.Run
import qualified Frontend
import qualified Backend

main :: IO ()
main = do
  args <- getArgs
  let port = read $ args !! 0
      assets = args !! 1
      profileFile = (args !! 2) <> ".rprof"
  Obelisk.Run.run port (Obelisk.Run.runServeAsset assets) Backend.backend Frontend.frontend `finally` writeProfilingData profileFile
  '';
in obelisk.nixpkgs.runCommand "ob-run" {
     buildInputs = [ (profiled.ghc.ghcWithPackages (p: [ p.backend p.frontend])) ];
} "ghc -x hs -prof -fno-prof-auto -threaded ${exeSource} -o $out"
|]

run
  :: MonadObelisk m
  => m ()
run = withProjectRoot "." $ \root -> do
  pkgs <- fmap toList . parsePackagesOrFail =<< getLocalPkgs root
  withGhciScript pkgs root $ \dotGhciPath -> do
    freePort <- getFreePort
    assets <- findProjectAssets root
    putLog Debug $ "Assets impurely loaded from: " <> assets
    runGhcid root True dotGhciPath pkgs $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(Obelisk.Run.runServeAsset " ++ show assets ++ ")"
      , "Backend.backend"
      , "Frontend.frontend"
      ]

runRepl :: MonadObelisk m => m ()
runRepl = withProjectRoot "." $ \root -> do
  pkgs <- fmap toList . parsePackagesOrFail =<< getLocalPkgs root
  withGhciScript pkgs "." $ \dotGhciPath ->
    runGhciRepl root pkgs dotGhciPath

runWatch :: MonadObelisk m => m ()
runWatch = withProjectRoot "." $ \root -> do
  pkgs <- fmap toList . parsePackagesOrFail =<< getLocalPkgs root
  withGhciScript pkgs root $ \dotGhciPath ->
    runGhcid root True dotGhciPath pkgs Nothing

-- | Relative paths to local packages of an obelisk project.
--
-- These are a combination of the obelisk predefined local packages,
-- and any packages that the user has set with the @packages@ argument
-- to the Nix @project@ function.
getLocalPkgs :: MonadObelisk m => FilePath -> m [FilePath]
getLocalPkgs root = do
  obeliskPaths <- runFind ["-L", root, "-name", ".obelisk", "-type", "d"]

  -- We do not want to find packages that are embedded inside other obelisk projects, unless that
  -- obelisk project is our own.
  let exclusions = filter (/= root) $ map takeDirectory obeliskPaths
  fmap (map (makeRelative ".")) $ runFind $
    ["-L", root, "(", "-name", "*.cabal", "-o", "-name", Hpack.packageConfig, ")", "-a", "-type", "f"]
    <> concat [["-not", "-path", p </> "*"] | p <- (toImplDir "*" </> attrCacheFileName) : exclusions]
  where
    runFind args = do
      (_exitCode, out, err) <- readCreateProcessWithExitCode $ proc findExePath args
      putLog Debug $ T.strip $ T.pack err
      pure $ map T.unpack $ T.lines $ T.strip $ T.pack out

data GuessPackageFileError = GuessPackageFileError_Ambiguous [FilePath] | GuessPackageFileError_NotFound
  deriving (Eq, Ord, Show)
instance Exception GuessPackageFileError

newtype HPackFilePath = HPackFilePath { unHPackFilePath :: FilePath } deriving (Eq, Ord, Show)
newtype CabalFilePath = CabalFilePath { unCabalFilePath :: FilePath } deriving (Eq, Ord, Show)

-- | Given a directory, try to guess what the appropriate @.cabal@ or @package.yaml@ file is for the package.
guessCabalPackageFile
  :: (MonadIO m)
  => FilePath -- ^ Directory or path to search for cabal package
  -> m (Either GuessPackageFileError (Either CabalFilePath HPackFilePath))
guessCabalPackageFile pkg = do
  liftIO (doesDirectoryExist pkg) >>= \case
    False -> case cabalOrHpackFile pkg of
      (Just hpack@(Right _)) -> pure $ Right hpack
      (Just cabal@(Left (CabalFilePath cabalFilePath))) -> do
        -- If the cabal file has a sibling hpack file, we use that instead
        -- since running hpack often generates a sibling cabal file
        let possibleHpackSibling = takeDirectory cabalFilePath </> Hpack.packageConfig
        hasHpackSibling <- liftIO $ doesFileExist possibleHpackSibling
        pure $ Right $ if hasHpackSibling then Right (HPackFilePath possibleHpackSibling) else cabal
      Nothing -> pure $ Left GuessPackageFileError_NotFound
    True -> do
      candidates <- liftIO $
            filterM (doesFileExist . either unCabalFilePath unHPackFilePath)
        =<< mapMaybe (cabalOrHpackFile . (pkg </>)) <$> listDirectory pkg
      pure $ case partitionEithers candidates of
        ([hpack], _) -> Right $ Left hpack
        ([], [cabal]) -> Right $ Right cabal
        ([], []) -> Left GuessPackageFileError_NotFound
        (hpacks, cabals) -> Left $ GuessPackageFileError_Ambiguous $ coerce hpacks <> coerce cabals

cabalOrHpackFile :: FilePath -> Maybe (Either CabalFilePath HPackFilePath)
cabalOrHpackFile = \case
  x | takeExtension x == ".cabal" -> Just (Left $ CabalFilePath x)
    | takeFileName x == Hpack.packageConfig -> Just (Right $ HPackFilePath x)
    | otherwise -> Nothing

-- | Parses the cabal package in a given directory.
-- This automatically figures out which .cabal file or package.yaml (hpack) file to use in the given directory.
parseCabalPackage
  :: MonadObelisk m
  => FilePath -- ^ Package directory
  -> m (Maybe CabalPackageInfo)
parseCabalPackage dir = parseCabalPackage' dir >>= \case
  Left err -> Nothing <$ putLog Error err
  Right (warnings, pkgInfo) -> do
    for_ warnings $ putLog Warning . T.pack . show
    pure $ Just pkgInfo

-- | Like 'parseCabalPackage' but returns errors and warnings directly so as to avoid 'MonadObelisk'.
parseCabalPackage'
  :: (MonadIO m)
  => FilePath -- ^ Package directory
  -> m (Either T.Text ([Dist.PWarning], CabalPackageInfo))
parseCabalPackage' pkg = runExceptT $ do
  (cabalContents, packageFile, packageName) <- guessCabalPackageFile pkg >>= \case
    Left GuessPackageFileError_NotFound -> throwError $ "No .cabal or package.yaml file found in " <> T.pack pkg
    Left (GuessPackageFileError_Ambiguous _) -> throwError $ "Unable to determine which .cabal file to use in " <> T.pack pkg
    Right (Left (CabalFilePath file)) -> (, file, takeBaseName file) <$> liftIO (readUTF8File file)
    Right (Right (HPackFilePath file)) -> do
      let
        decodeOptions = Hpack.DecodeOptions (Hpack.ProgramName "ob") file Nothing Hpack.decodeYaml
      liftIO (Hpack.readPackageConfig decodeOptions) >>= \case
        Left err -> throwError $ T.pack $ "Failed to parse " <> file <> ": " <> err
        Right (Hpack.DecodeResult hpackPackage _ _ _) -> pure (Hpack.renderPackage [] hpackPackage, file, Hpack.packageName hpackPackage)

  let
    (warnings, result) = runParseResult $ parseGenericPackageDescription $ toUTF8BS cabalContents
    osConfVar = case System.Info.os of
      "linux" -> Just Dist.Linux
      "darwin" -> Just Dist.OSX
      _ -> trace "Unrecgonized System.Info.os" Nothing
    archConfVar = Just Dist.X86_64 -- TODO: Actually infer this
    evalConfVar v = Right $ case v of
      OS osVar -> Just osVar == osConfVar
      Arch archVar -> Just archVar == archConfVar
      Impl GHC _ -> True -- TODO: Actually check version range
      _ -> False
  case condLibrary <$> result of
    Right (Just condLib) -> do
      let (_, lib) = simplifyCondTree evalConfVar condLib
      pure $ (warnings,) $ CabalPackageInfo
        { _cabalPackageInfo_packageName = T.pack packageName
        , _cabalPackageInfo_packageFile = packageFile
        , _cabalPackageInfo_packageRoot = takeDirectory packageFile
        , _cabalPackageInfo_buildable = buildable $ libBuildInfo lib
        , _cabalPackageInfo_sourceDirs =
            fromMaybe (pure ".") $ NE.nonEmpty $ hsSourceDirs $ libBuildInfo lib
        , _cabalPackageInfo_defaultExtensions =
            defaultExtensions $ libBuildInfo lib
        , _cabalPackageInfo_defaultLanguage =
            defaultLanguage $ libBuildInfo lib
        , _cabalPackageInfo_compilerOptions = options $ libBuildInfo lib
        }
    Right Nothing -> throwError "Haskell package has no library component"
    Left (_, errors) ->
      throwError $ T.pack $ "Failed to parse " <> packageFile <> ":\n" <> unlines (map show errors)

parsePackagesOrFail :: MonadObelisk m => [FilePath] -> m (NE.NonEmpty CabalPackageInfo)
parsePackagesOrFail dirs = do
  (pkgDirErrs, packageInfos') <- fmap partitionEithers $ for dirs $ \dir -> do
    flip fmap (parseCabalPackage dir) $ \case
      Just packageInfo
        | _cabalPackageInfo_buildable packageInfo -> Right packageInfo
      _ -> Left dir

  let packagesByName = Map.fromListWith (<>) [(_cabalPackageInfo_packageName p, p NE.:| []) | p <- packageInfos']
  unambiguous <- ifor packagesByName $ \packageName ps -> case ps of
    p NE.:| [] -> pure p -- No ambiguity here
    p NE.:| _ -> do
      putLog Warning $ T.pack $
        "Packages named '" <> T.unpack packageName <> "' appear in " <> show (length ps) <> " different locations: "
        <> intercalate ", " (map _cabalPackageInfo_packageFile $ toList ps)
        <> "; Picking " <> _cabalPackageInfo_packageFile p
      pure p

  packageInfos <- case NE.nonEmpty $ toList unambiguous of
    Nothing -> failWith $ T.pack $ "No valid, buildable packages found in " <> intercalate ", " dirs
    Just xs -> pure xs

  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find buildable packages in " <> intercalate ", " pkgDirErrs

  pure packageInfos

packageInfoToNamePathMap :: [CabalPackageInfo] -> Map Text FilePath
packageInfoToNamePathMap = Map.fromList . map (_cabalPackageInfo_packageName &&& _cabalPackageInfo_packageRoot)

-- | Create ghci configuration to load the given packages
withGhciScript
  :: MonadObelisk m
  => [CabalPackageInfo] -- ^ List of packages to load into ghci
  -> FilePath -- ^ All paths written to the .ghci file will be relative to this path
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporary .ghci
  -> m ()
withGhciScript packageInfos pathBase f = do
  selfExe <- liftIO getExecutablePath
  let
    packageNames = Set.fromList $ map _cabalPackageInfo_packageName packageInfos
    modulesToLoad = mconcat
      [ [ "Obelisk.Run" | "obelisk-run" `Set.member` packageNames ]
      , [ "Backend" | "backend" `Set.member` packageNames ]
      , [ "Frontend" | "frontend" `Set.member` packageNames ]
      ]
    dotGhci = unlines
      -- TODO: Shell escape
      [ ":set -F -pgmF " <> selfExe <> " -optF " <> preprocessorIdentifier <> " " <> unwords (map (("-optF " <>) . makeRelative pathBase . _cabalPackageInfo_packageFile) packageInfos)
      , ":set -i" <> intercalate ":" (packageInfos >>= rootedSourceDirs)
      , if null modulesToLoad then "" else ":load " <> unwords modulesToLoad
      , "import qualified Obelisk.Run"
      , "import qualified Frontend"
      , "import qualified Backend" ]
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath

  where
    rootedSourceDirs pkg = NE.toList $
      makeRelative pathBase . (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg

-- | Run ghci repl
runGhciRepl
  :: MonadObelisk m
  => FilePath -- ^ Path to project root
  -> [CabalPackageInfo]
  -> FilePath -- ^ Path to .ghci
  -> m ()
runGhciRepl root packages dotGhci =
  -- NOTE: We do *not* want to use $(staticWhich "ghci") here because we need the
  -- ghc that is provided by the shell in the user's project.
  nixShellWithPkgs root True False (packageInfoToNamePathMap packages) $ Just $ "ghci " <> makeBaseGhciOptions dotGhci -- TODO: Shell escape

-- | Run ghcid
runGhcid
  :: MonadObelisk m
  => FilePath -- ^ Path to project root
  -> Bool -- ^ Should we chdir to root when running this process?
  -> FilePath -- ^ Path to .ghci
  -> [CabalPackageInfo]
  -> Maybe String -- ^ Optional command to run at every reload
  -> m ()
runGhcid root chdirToRoot dotGhci packages mcmd =
  nixShellWithPkgs root True chdirToRoot (packageInfoToNamePathMap packages) (Just $ unwords $ ghcidExePath : opts) -- TODO: Shell escape
  where
    opts =
      [ "-W"
      , "--command='ghci -ignore-dot-ghci " <> makeBaseGhciOptions dotGhci <> "' "
      , "--outputfile=ghcid-output.txt"
      ] <> map (\x -> "--reload='" <> x <> "'") reloadFiles
        <> map (\x -> "--restart='" <> x <> "'") restartFiles
        <> testCmd
    testCmd = maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'") -- TODO: Shell escape

    adjustRoot x = if chdirToRoot then makeRelative root x else x
    reloadFiles = map adjustRoot [root </> "config"]
    restartFiles = map (adjustRoot . _cabalPackageInfo_packageFile) packages

makeBaseGhciOptions :: FilePath -> String
makeBaseGhciOptions dotGhci =
  unwords
    [ "-no-user-package-db"
    , "-package-env -"
    , "-ghci-script " <> dotGhci
    ]

getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock

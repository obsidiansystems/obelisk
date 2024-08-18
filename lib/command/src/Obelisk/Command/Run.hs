{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

module Obelisk.Command.Run where

import Control.Arrow ((&&&))
import Control.Exception (Exception, bracket)
import Control.Lens (ifor, (.~), (&), view)
import Control.Concurrent (forkIO)
import Control.Monad (filterM, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Bifoldable (bifoldr1)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Foldable (fold, for_, toList)
import Data.Functor.Identity (runIdentity)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import Data.Either
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Here.Interpolated (i)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Traversable (for)
import Debug.Trace (trace)
#if MIN_VERSION_Cabal(3,4,0)
import qualified Distribution.Compat.NonEmptySet as CabalSet
#endif
#if MIN_VERSION_Cabal(3,2,1)
import Distribution.Compiler (CompilerFlavor(..), perCompilerFlavorToList, PerCompilerFlavor)
#else
import Distribution.Compiler (CompilerFlavor(..), PerCompilerFlavor)
#endif
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
#if MIN_VERSION_Cabal(3,2,1)
import Distribution.Fields.ParseResult (runParseResult)
#else
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
#endif
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Compiler (PackageDB (GlobalPackageDB))
import Distribution.Simple.Configure (configCompilerEx, getInstalledPackages)
import Distribution.Simple.PackageIndex (InstalledPackageIndex, lookupDependency)
import Distribution.Simple.Program.Db (defaultProgramDb)
import qualified Distribution.System as Dist
import Distribution.Types.BuildInfo (buildable, cppOptions, defaultExtensions, defaultLanguage, hsSourceDirs, options, targetBuildDepends)
import Distribution.Types.CondTree (simplifyCondTree)
import Distribution.Types.Dependency (Dependency (..), depPkgName)
import Distribution.Parsec.Warning (PWarning)
#if MIN_VERSION_Cabal(3,2,1)
import Distribution.Types.GenericPackageDescription.Lens (ConfVar (Arch, Impl, OS), condLibrary)
#else
import Distribution.Types.GenericPackageDescription (condLibrary)
import Distribution.Types.ConfVar (ConfVar (Arch, Impl, OS))
#endif
import Distribution.Types.InstalledPackageInfo (compatPackageKey)
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.LibraryName (LibraryName(..))
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.VersionRange (anyVersion)
import Distribution.Utils.Generic (toUTF8BS, readUTF8File)
#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (getSymbolicPath)
#endif
#if MIN_VERSION_Cabal(3,2,1)
import qualified Distribution.Parsec.Warning as Dist
#else
import qualified Distribution.System as Dist
#endif
import Distribution.Types.Dependency (Dependency (..), depPkgName, depVerRange)
import qualified Distribution.Verbosity as Verbosity (silent)
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import qualified Hpack.Yaml as Hpack
import Language.Haskell.Extension (Extension, Language)
import qualified Network.Socket as Socket
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import qualified System.Info
import System.IO.Temp (withSystemTempDirectory)

import Obelisk.App (MonadObelisk, ObeliskError(..), getObelisk, runObelisk)
import Obelisk.Command.Nix
import Obelisk.Command.Project
import Obelisk.Command.Utils (findExePath, ghcidExePath)
import "nix-thunk" Nix.Thunk
import Cli.Extras

#if MIN_VERSION_Cabal(3,4,0)
cabalSetSingleton :: a -> CabalSet.NonEmptySet a
cabalSetSingleton = CabalSet.singleton
#else
cabalSetSingleton :: a -> Set a
cabalSetSingleton = Set.singleton
#endif

#if !MIN_VERSION_Cabal(3,6,0)
getSymbolicPath :: a -> a
getSymbolicPath = id
#endif

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
  , _cabalPackageInfo_compilerOptions :: PerCompilerFlavor [String]
    -- ^ List of compiler-specific options (e.g., the "ghc-options" field of the cabal file)
  , _cabalPackageInfo_cppOptions :: [String]
    -- ^ List of CPP (C Preprocessor) options (e.g. the "cpp-options" field of the cabal file)
  , _cabalPackageInfo_buildDepends :: [Dependency]
    -- ^ List of build dependencies listed in the cabal file
  }

-- | 'Bool' with a better name for its purpose.
data Interpret = Interpret_Interpret | Interpret_NoInterpret deriving (Eq, Ord, Show)

textInterpret :: Interpret -> Text
textInterpret = \case
  Interpret_Interpret -> "Interpret"
  Interpret_NoInterpret -> "NoInterpret"

-- | Describe a set of 'FilePath's as a tree to facilitate merging them in a convenient way.
data PathTree a = PathTree_Node
  (Maybe a) -- An optional leaf at this point in the tree
  (Map FilePath (PathTree a)) -- Branches to deeper leaves
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- | 2D ASCII drawing of a 'PathTree'. Adapted from Data.Tree.draw.
drawPathTree :: (a -> Text) -> PathTree a -> Text
drawPathTree showA (PathTree_Node _ ts0) = T.intercalate "\n" $ goForest (Map.toList ts0)
  where
    annotated ma = maybe id (\a b -> b <> " [" <> showA a <> "]") ma . T.pack
    goTree (fp, PathTree_Node ma forest) = annotated ma fp : goForest (Map.toList forest)
    goForest [] = []
    goForest [tree] = shift "└─ " "   " (goTree tree)
    goForest (tree:forest) = shift "├─ " "│  " (goTree tree) <> goForest forest
    shift first other = zipWith (<>) (first : repeat other)

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

  outPath <- withSpinner "Building profiled executable" $
    fmap (T.unpack . T.strip) $ readProcessAndLogStderr Debug $ setCwd (Just root) $ nixCmdProc $
      NixCmd_Build $ def
        & nixBuildConfig_outLink .~ OutLink_None
        & nixCmdConfig_target .~ Target
          { _target_path = Just "."
          , _target_attr = Just "__unstable__.profiledObRun"
          , _target_expr = Nothing
          }
  (assetType, assets) <- findProjectAssets root
  putLog Debug $ describeImpureAssetSource assetType assets
  time <- liftIO getCurrentTime
  let profileBaseName = formatTime defaultTimeLocale profileBasePattern time
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ root </> profileBaseName
  putLog Debug $ "Storing profiled data under base name of " <> T.pack (root </> profileBaseName)
  freePort <- getFreePort
  runProcess_ $ setCwd (Just root) $ setDelegateCtlc True $ proc (outPath </> "bin" </> "ob-run") $
    [ show freePort
    , T.unpack assets
    , profileBaseName
    , "+RTS"
    , "-po" <> profileBaseName
    ] <> rtsFlags
      <> [ "-RTS" ]

run
  :: MonadObelisk m
  => Maybe FilePath
  -- ^ Certificate Directory path (optional)
  -> Maybe Socket.PortNumber
  -- ^ override the route's port number?
  -> FilePath
  -- ^ root folder
  -> PathTree Interpret
  -- ^ interpreted paths
  -> m ()
run certDir portOverride root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  (assetType, assets) <- findProjectAssets root
  manifestPkg <- parsePackagesOrFail . (:[]) . T.unpack =<< getHaskellManifestProjectPath root
  putLog Debug $ describeImpureAssetSource assetType assets
  case assetType of
    AssetSource_Derivation -> do
      ob <- getObelisk
      putLog Debug "Starting static file derivation watcher..."
      void $ liftIO $ forkIO $ runObelisk ob $ watchStaticFilesDerivation root
    _ -> pure ()
  ghciArgs <- getGhciSessionSettings (pkgs <> manifestPkg) root
  freePort <- getFreePort
  withGhciScriptArgs [] pkgs $ \dotGhciArgs -> do
    runGhcid root True (ghciArgs <> dotGhciArgs) pkgs $ Just $ unwords
      [ "Obelisk.Run.run (Obelisk.Run.defaultRunApp"
      , "Backend.backend"
      , "Frontend.frontend"
      , "(Obelisk.Run.runServeAsset " ++ show assets ++ ")"
      , ") { Obelisk.Run._runApp_backendPort =", show freePort
      ,   ", Obelisk.Run._runApp_forceFrontendPort =", show portOverride
      ,   ", Obelisk.Run._runApp_tlsCertDirectory =", show certDir
      , "}"
      ]

runRepl :: MonadObelisk m => Maybe FilePath -> FilePath -> PathTree Interpret -> m ()
runRepl mUserGhciConfig root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs root
  userCommands <- maybe (pure []) (fmap lines . liftIO . readFile) mUserGhciConfig
  withGhciScriptArgs userCommands pkgs $ \dotGhciArgs ->
    runGhciRepl root pkgs (ghciArgs <> dotGhciArgs)

runWatch :: MonadObelisk m => FilePath -> PathTree Interpret -> m ()
runWatch root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs root
  withGhciScriptArgs [] pkgs $ \dotGhciArgs ->
    runGhcid root True (ghciArgs <> dotGhciArgs) pkgs Nothing

exportGhciConfig :: MonadObelisk m => FilePath -> PathTree Interpret -> m [String]
exportGhciConfig root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  getGhciSessionSettings pkgs root

nixShellForInterpretPaths :: MonadObelisk m => Bool -> String -> FilePath -> PathTree Interpret -> Maybe String -> m ()
nixShellForInterpretPaths isPure shell' root interpretPaths cmd = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  nixShellWithoutPkgs root isPure False (packageInfoToNamePathMap pkgs) shell' cmd

-- | Like 'getLocalPkgs' but also parses them and fails if any of them can't be parsed.
getParsedLocalPkgs :: MonadObelisk m => FilePath -> PathTree Interpret -> m (NonEmpty CabalPackageInfo)
getParsedLocalPkgs root interpretPaths = parsePackagesOrFail =<< getLocalPkgs root interpretPaths

-- | Relative paths to local packages of an obelisk project.
--
-- These are a combination of the obelisk predefined local packages,
-- and any packages that the user has set with the @packages@ argument
-- to the Nix @project@ function.
getLocalPkgs :: forall m. MonadObelisk m => FilePath -> PathTree Interpret -> m (Set FilePath)
getLocalPkgs root interpretPaths = do
  putLog Debug $ [i|Finding packages with root ${root} and interpret paths:|] <> "\n" <> drawPathTree textInterpret interpretPaths
  obeliskPackagePaths <- runFind ["-L", root, "-name", ".obelisk", "-type", "d"]

  -- We do not want to find packages that are embedded inside other obelisk projects, unless that
  -- obelisk project is our own.
  obeliskPackageExclusions <- liftIO $ fmap Set.fromList $ traverse canonicalizePath $
    filter (/= root) $ map takeDirectory obeliskPackagePaths
  putLog Debug [i|Excluding obelisk packages: ${T.pack $ unwords $ Set.toList obeliskPackageExclusions}|]
  let rootsAndExclusions = calcIntepretFinds "" interpretPaths

  fmap fold $ for (MMap.toAscList rootsAndExclusions) $ \(interpretPathRoot, exclusions) ->
    let allExclusions = obeliskPackageExclusions
          <> exclusions
          <> Set.singleton ("*" </> attrCacheFileName)
          <> Set.singleton ("*" </> "lib/asset/manifest") -- NB: obelisk-asset-manifest is excluded because it generates
                                                          -- a module that in turn imports it. This will cause ob run to
                                                          -- fail in its current implementation.
    in fmap (Set.fromList . map normalise) $ runFind $
      ["-L", interpretPathRoot, "(", "-name", "*.cabal", "-o", "-name", Hpack.packageConfig, ")", "-a", "-type", "f"]
      <> concat [["-not", "-path", p </> "*"] | p <- toList allExclusions]
  where
    runFind args = do
      (_exitCode, out, err) <- readCreateProcessWithExitCode $ proc findExePath args
      putLog Debug $ T.strip $ T.pack err
      pure $ map T.unpack $ T.lines $ T.strip $ T.pack out

-- | Calculates a set of root 'FilePath's along with each one's corresponding set of exclusions.
--   This is used when constructing a set of @find@ commands to run to produce a set of packages
--   that matches the user's @--interpret@/@--no-interpret@ settings.
calcIntepretFinds :: FilePath -> PathTree Interpret -> MMap.MonoidalMap FilePath (Set FilePath)
calcIntepretFinds treeRoot0 tree0 = runIdentity $ go treeRoot0 tree0
  where
    go treeRoot tree = foldPathTreeFor (== Interpret_Interpret) treeRoot tree $ \parent children -> do
      exclusions <- foldPathTreeFor (== Interpret_NoInterpret) parent children $ \parent' children' ->
        pure $ Map.singleton parent' children'
      deeperFinds <- Map.traverseWithKey go exclusions
      pure $ MMap.singleton parent (Map.keysSet exclusions) <> fold (MMap.MonoidalMap deeperFinds)

-- | Traverses a 'PathTree' and folds all leaves matching a given predicate.
foldPathTreeFor
  :: forall m a b. (Applicative m, Monoid b)
  => (a -> Bool)
  -> FilePath
  -> PathTree a
  -> (FilePath -> PathTree a -> m b)
  -> m b
foldPathTreeFor predicate parent children f = case children of
  PathTree_Node (Just x) children' | predicate x -> f parent (PathTree_Node Nothing children')
  PathTree_Node _ children' -> fmap fold $ flip Map.traverseWithKey children' $ \k children'' ->
    foldPathTreeFor predicate (parent </> k) children'' f

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
  Left err -> throwError (ObeliskError_Unstructured err)
  Right (Just (warnings, pkgInfo)) -> do
    for_ warnings $ putLog Warning . T.pack . show
    pure $ Just pkgInfo
  Right Nothing -> pure Nothing

-- | Like 'parseCabalPackage' but returns errors and warnings directly so as to avoid 'MonadObelisk'.
parseCabalPackage'
  :: (MonadIO m)
  => FilePath -- ^ Package directory
  -> m (Either T.Text (Maybe ([PWarning], CabalPackageInfo)))
parseCabalPackage' pkg = runExceptT $ do
  (cabalContents, packageFile, packageName) <- guessCabalPackageFile pkg >>= \case
    Left GuessPackageFileError_NotFound -> throwError $ "No .cabal or package.yaml file found in " <> T.pack pkg
    Left (GuessPackageFileError_Ambiguous _) -> throwError $ "Unable to determine which .cabal file to use in " <> T.pack pkg
    Right (Left (CabalFilePath file)) -> (, file, takeBaseName file) <$> liftIO (readUTF8File file)
    Right (Right (HPackFilePath file)) -> do
      let
        decodeOptions = Hpack.defaultDecodeOptions
          { Hpack.decodeOptionsProgramName = Hpack.ProgramName "ob"
          , Hpack.decodeOptionsTarget = file
          , Hpack.decodeOptionsUserDataDir = Nothing
          , Hpack.decodeOptionsDecode = Hpack.decodeYaml
          }
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
#if MIN_VERSION_Cabal(3,2,1)
  case (view condLibrary) <$> result of
#else
  case condLibrary <$> result of
#endif
    Right (Just condLib) -> do
      let (_, lib) = simplifyCondTree evalConfVar condLib
      pure $ Just $ (warnings,) $ CabalPackageInfo
        { _cabalPackageInfo_packageName = T.pack packageName
        , _cabalPackageInfo_packageFile = packageFile
        , _cabalPackageInfo_packageRoot = takeDirectory packageFile
        , _cabalPackageInfo_buildable = buildable $ libBuildInfo lib
        , _cabalPackageInfo_sourceDirs =
            fromMaybe (pure ".") $ NE.nonEmpty $ fmap getSymbolicPath $ hsSourceDirs $ libBuildInfo lib
        , _cabalPackageInfo_defaultExtensions =
            defaultExtensions $ libBuildInfo lib
        , _cabalPackageInfo_defaultLanguage =
            defaultLanguage $ libBuildInfo lib
        , _cabalPackageInfo_compilerOptions =
            options $ libBuildInfo lib
        , _cabalPackageInfo_cppOptions = cppOptions $ libBuildInfo lib
        , _cabalPackageInfo_buildDepends = targetBuildDepends $ libBuildInfo lib
        }
    Right Nothing -> pure Nothing
    Left (_, errors) ->
      throwError $ T.pack $ "Failed to parse " <> packageFile <> ":\n" <> unlines (map show $ toList errors)

parsePackagesOrFail :: (MonadObelisk m, Foldable f) => f FilePath -> m (NE.NonEmpty CabalPackageInfo)
parsePackagesOrFail dirs' = do
  packageInfos' <- fmap catMaybes $ for dirs $ \dir -> do
    flip fmap (parseCabalPackage dir) $ \case
      Just packageInfo
        | _cabalPackageInfo_buildable packageInfo -> Just packageInfo
      _ -> Nothing

  -- Sort duplicate packages such that we prefer shorter paths, but fall back to alphabetical ordering.
  let packagesByName = Map.map (NE.sortBy $ comparing $ \p -> let n = _cabalPackageInfo_packageFile p in (length n, n))
                     $ Map.fromListWith (<>) [(_cabalPackageInfo_packageName p, p NE.:| []) | p <- packageInfos']
  unambiguous <- ifor packagesByName $ \packageName ps -> case ps of
    p NE.:| [] -> pure p -- No ambiguity here
    p NE.:| _ -> do
      let chosenText = "  [Chosen] "
          prefix p'
            | _cabalPackageInfo_packageFile p' == _cabalPackageInfo_packageFile p = chosenText
            | otherwise = T.map (const ' ') chosenText
      putLog Warning $ T.unlines $
        "Packages named '" <> packageName <> "' appear in " <> T.pack (show $ length ps) <> " different locations: "
        : map (\p' -> prefix p' <> T.pack (_cabalPackageInfo_packageFile p')) (toList ps)
      pure p

  packageInfos <- case NE.nonEmpty $ toList unambiguous of
    Nothing -> failWith $ T.pack $
      "No valid, buildable packages found" <> (if null dirs then "" else " in " <> intercalate ", " dirs)
    Just xs -> pure xs

  pure packageInfos
  where
    dirs = toList dirs'

packageInfoToNamePathMap :: Foldable f => f CabalPackageInfo -> Map Text FilePath
packageInfoToNamePathMap = Map.fromList . map (_cabalPackageInfo_packageName &&& _cabalPackageInfo_packageRoot) . toList

-- Like 'withGhciScript' but provides the precise ghci arguments to add to a ghci session
withGhciScriptArgs
  :: (MonadObelisk m, Foldable f)
  => [String] -- ^ User commands to insert into .ghci
  -> f CabalPackageInfo -- ^ List of packages to load into ghci
  -> ([String] -> m ()) -- ^ Action to run with the extra ghci arguments
  -> m ()
withGhciScriptArgs userCommands packageInfos f =
  withGhciScript (loadPreludeManually ++ userCommands) packageInfos $ \fp ->
    f ["-XNoImplicitPrelude", "-ghci-script", fp]
  where
    -- These lines must be first and allow the session to support a custom Prelude when @-XNoImplicitPrelude@
    -- is passed to the ghci session.
    loadPreludeManually =
      [ ":add Prelude" -- @:add@ is used because it's less noisy when there is no custom Prelude
      , ":set -XImplicitPrelude" -- Turn the default setting on
      ]

-- | Create ghci configuration to load the given packages
withGhciScript
  :: (MonadObelisk m, Foldable f)
  => [String] -- ^ Commands to prefix to file
  -> f CabalPackageInfo -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporary .ghci
  -> m ()
withGhciScript preCommands (toList -> packageInfos) f =
  withSystemTempDirectory "ob-ghci" $ \fp -> do
    let dotGhciPath = fp </> ".ghci"
    liftIO $ writeFile dotGhciPath dotGhci
    f dotGhciPath
  where
    packageNames = Set.fromList $ map _cabalPackageInfo_packageName packageInfos
    modulesToLoad = mconcat
      [ [ "Obelisk.Run" | "obelisk-run" `Set.member` packageNames ]
      , [ "Backend" | "backend" `Set.member` packageNames ]
      , [ "Frontend" | "frontend" `Set.member` packageNames ]
      ]
    dotGhci = unlines $
      preCommands <>
      [ if null modulesToLoad then "" else ":load " <> unwords modulesToLoad
      , "import qualified Obelisk.Run"
      , "import qualified Frontend"
      , "import qualified Backend"
      ]

-- | Builds a list of options to pass to ghci or set in .ghci file that configures
-- the preprocessor and source includes.
getGhciSessionSettings
  :: (MonadObelisk m, Foldable f)
  => f CabalPackageInfo -- ^ List of packages to load into ghci
  -> FilePath -- ^ All paths will be relative to this path
  -> m [String]
getGhciSessionSettings (toList -> packageInfos) pathBase = do
  selfExe <- liftIO $ canonicalizePath =<< getExecutablePath
  installedPackageIndex <- loadPackageIndex packageInfos pathBase

  (pkgFiles, pkgSrcPaths :: [NonEmpty FilePath]) <- fmap unzip $ liftIO $ for packageInfos $ \pkg -> do
    canonicalSrcDirs <- traverse canonicalizePath $ (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg
    canonicalPkgFile <- canonicalizePath $ _cabalPackageInfo_packageFile pkg
    pure (canonicalPkgFile, canonicalSrcDirs)

  pure
    $  baseGhciOptions
    <> ["-DOBELISK_ASSET_PASSTHRU"] -- For passthrough static assets
    <> ["-F", "-pgmF", selfExe, "-optF", preprocessorIdentifier]
    <> concatMap (\p -> ["-optF", p]) pkgFiles
    <> ["-i" <> intercalate ":" (concatMap toList pkgSrcPaths)]
    <> concatMap (\packageId -> ["-package-id", packageId ])
                 (packageIds installedPackageIndex)
  where
    -- Package names we're building and not needed from the package DB
    packageNames =
      map (mkPackageName . T.unpack . _cabalPackageInfo_packageName)
          packageInfos
    packageIds installedPackageIndex = Set.toList $ Set.fromList $
      map (dependencyPackageId installedPackageIndex) $
          filter ((`notElem` packageNames) . depPkgName) $
          concatMap _cabalPackageInfo_buildDepends packageInfos <>
            [Dependency (mkPackageName "obelisk-run") anyVersion (cabalSetSingleton LMainLibName)]
    dependencyPackageId installedPackageIndex dep =
      case lookupDependency installedPackageIndex (depPkgName dep) (depVerRange dep) of
        ((_version,installedPackageInfo:_) :_) ->
          compatPackageKey installedPackageInfo
        _ -> error $ "Couldn't resolve dependency for " <> prettyShow dep


-- Load the package index used by the GHC in this path's nix project
loadPackageIndex :: MonadObelisk m => [CabalPackageInfo] -> FilePath -> m InstalledPackageIndex
loadPackageIndex packageInfos root = do
  ghcPath <- getPathInNixEnvironment "bash -c 'type -p ghc'"
  ghcPkgPath <- getPathInNixEnvironment "bash -c 'type -p ghc-pkg'"
  (compiler, _platform, programDb) <- liftIO
    $ configCompilerEx (Just GHC) (Just ghcPath) (Just ghcPkgPath) defaultProgramDb Verbosity.silent
  liftIO $ getInstalledPackages Verbosity.silent compiler [GlobalPackageDB] programDb
  where
    getPathInNixEnvironment cmd = do
      path <- readProcessAndLogStderr Debug =<< mkObNixShellProc root False True (packageInfoToNamePathMap packageInfos) "ghc" (Just cmd)
      liftIO $ canonicalizePath $ T.unpack $ T.strip path

baseGhciOptions :: [String]
baseGhciOptions =
  [ "-ignore-dot-ghci"
  , "-no-user-package-db"
  , "-hide-all-packages"
  , "-package-env", "-"
  ]

-- | Run ghci repl
runGhciRepl
  :: (MonadObelisk m, Foldable f)
  => FilePath -- ^ Path to project root
  -> f CabalPackageInfo -- ^ Packages to keep unbuilt
  -> [String] -- ^ GHCi arguments
  -> m ()
runGhciRepl root (toList -> packages) ghciArgs =
  -- NOTE: We do *not* want to use $(staticWhich "ghci") here because we need the
  -- ghc that is provided by the shell in the user's project.
  nixShellWithoutPkgs root True True (packageInfoToNamePathMap packages) "ghc" $
    Just $ unwords $ fmap bashEscape $ "ghci" : ghciArgs

-- | Run ghcid
runGhcid
  :: (MonadObelisk m, Foldable f)
  => FilePath -- ^ Path to project root
  -> Bool -- ^ Should we chdir to root when running this process?
  -> [String] -- ^ GHCi arguments
  -> f CabalPackageInfo -- ^ Packages to keep unbuilt
  -> Maybe String -- ^ Optional command to run at every reload
  -> m ()
runGhcid root chdirToRoot ghciArgs (toList -> packages) mcmd =
  nixShellWithoutPkgs root True chdirToRoot (packageInfoToNamePathMap packages) "ghc" $
    Just $ unwords $ fmap bashEscape $ ghcidExePath : opts
  where
    opts = concat
      [ ["-W"]
      , ["--outputfile=ghcid-output.txt"]
      , map (\x -> "--reload=" <> x) reloadFiles
      , map (\x -> "--restart=" <> x) restartFiles
      , maybe [] (\cmd -> ["--test=" <> cmd]) mcmd
      -- N.B. the subcommand to ghcid has to be itself escaped.
      -- We have to use 'shEscape' instead of 'bashEscape' because
      -- ghcid invokes System.Process with a shell command, which uses @\/bin\/sh@
      -- instead of the @bash@ we have in scope.
      -- This is not guaranteed to be bash on non-NixOS systems.
      , ["--command=" <> unwords (fmap shEscape ("ghci" : ghciArgs))]
      ]
    adjustRoot x = if chdirToRoot then makeRelative root x else x
    reloadFiles = map adjustRoot [root </> "config"]
    restartFiles = map (adjustRoot . _cabalPackageInfo_packageFile) packages

getFreePort :: MonadIO m => m Socket.PortNumber
getFreePort = liftIO $ Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) Socket.close Socket.socketPort
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      return sock


-- | Convert a 'FilePath' into a 'PathTree'.
pathToTree :: a -> FilePath -> PathTree a
pathToTree a p = go $ splitDirectories p
  where
    go [] = PathTree_Node (Just a) mempty
    go (x : xs) = PathTree_Node Nothing $ Map.singleton x $ go xs

-- | Like 'zipWith' but pads with a padding value instead of stopping on the shortest list.
zipDefaultWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipDefaultWith _da _db _f []     []     = []
zipDefaultWith  da  db  f (a:as) []     = f  a db : zipDefaultWith da db f as []
zipDefaultWith  da  db  f []     (b:bs) = f da  b : zipDefaultWith da db f [] bs
zipDefaultWith  da  db  f (a:as) (b:bs) = f  a  b : zipDefaultWith da db f as bs

-- | Makes the first absolute path relative to the second absolute path.
--
-- Both input paths MUST be absolute.
--
-- Unlike 'makeRelative' this does not merely strip prefixes. It will introduce
-- enough @..@ paths to make the resulting path truly relative in virtually every
-- case. The only exception is on Windows when the two paths are on different
-- drives. In this case the resulting path may be absolute.
relativeTo :: FilePath -> FilePath -> FilePath
relativeTo dir base
  = bifoldr1 (</>)
  $ bimap (collapse . (".." <$) . catMaybes) (collapse . catMaybes)
  $ unzip
  $ dropWhile (\(a,b) -> a == b)
  $ zipDefaultWith Nothing Nothing (,)
    (map Just $ splitDirectories base)
    (map Just $ splitDirectories dir)
  where collapse = foldr (</>) ""

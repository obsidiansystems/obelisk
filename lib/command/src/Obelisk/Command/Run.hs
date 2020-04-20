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
module Obelisk.Command.Run where

import Control.Arrow ((&&&))
import Control.Exception (Exception, bracket)
import Control.Lens (ifor, (.~), (&))
import Control.Monad (filterM, unless, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Bifoldable (bifoldr1)
import Data.Bifunctor (bimap)
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.Foldable (fold, for_, toList)
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Monoidal as MMap
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String.Here.Interpolated (i)
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
import Obelisk.CliApp (
    Severity (..),
    createProcess_,
    failWith,
    proc,
    putLog,
    readCreateProcessWithExitCode,
    readProcessAndLogStderr,
    setCwd,
    setDelegateCtlc,
    waitForProcess,
    withSpinner,
  )
import Obelisk.Command.Nix
import Obelisk.Command.Project (nixShellWithoutPkgs, withProjectRoot, findProjectAssets)
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

-- | 'Bool' with a better name for it's purpose.
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

emptyPathTree :: PathTree a
emptyPathTree = PathTree_Node Nothing mempty

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
  assets <- findProjectAssets root
  putLog Debug $ "Assets impurely loaded from: " <> assets
  time <- liftIO getCurrentTime
  let profileBaseName = formatTime defaultTimeLocale profileBasePattern time
  liftIO $ createDirectoryIfMissing True $ takeDirectory $ root </> profileBaseName
  putLog Debug $ "Storing profiled data under base name of " <> T.pack (root </> profileBaseName)
  freePort <- getFreePort
  (_, _, _, ph) <- createProcess_ "runProfExe" $ setCwd (Just root) $ setDelegateCtlc True $ proc (outPath </> "bin" </> "ob-run") $
    [ show freePort
    , T.unpack assets
    , profileBaseName
    , "+RTS"
    , "-po" <> profileBaseName
    ] <> rtsFlags
      <> [ "-RTS" ]
  void $ waitForProcess ph

run :: MonadObelisk m => FilePath -> PathTree Interpret -> m ()
run root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  withGhciScript pkgs $ \dotGhciPath -> do
    assets <- findProjectAssets root
    putLog Debug $ "Assets impurely loaded from: " <> assets
    ghciArgs <- getGhciSessionSettings pkgs root
    freePort <- getFreePort
    runGhcid root True (ghciArgs <> mkGhciScriptArg dotGhciPath) pkgs $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(Obelisk.Run.runServeAsset " ++ show assets ++ ")"
      , "Backend.backend"
      , "Frontend.frontend"
      ]

runRepl :: MonadObelisk m => FilePath -> PathTree Interpret -> m ()
runRepl root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs "."
  withGhciScript pkgs $ \dotGhciPath ->
    runGhciRepl root pkgs (ghciArgs <> mkGhciScriptArg dotGhciPath)

runWatch :: MonadObelisk m => FilePath -> PathTree Interpret -> m ()
runWatch root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs root
  withGhciScript pkgs $ \dotGhciPath ->
    runGhcid root True (ghciArgs <> mkGhciScriptArg dotGhciPath) pkgs Nothing

exportGhciConfig :: MonadObelisk m => FilePath -> PathTree Interpret -> m [String]
exportGhciConfig root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  getGhciSessionSettings pkgs "."

nixShellForInterpretPaths :: MonadObelisk m => Bool -> String -> FilePath -> PathTree Interpret -> Maybe String -> m ()
nixShellForInterpretPaths isPure shell root interpretPaths cmd = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  nixShellWithoutPkgs root isPure False (packageInfoToNamePathMap pkgs) shell cmd

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
    let allExclusions = obeliskPackageExclusions <> exclusions <> Set.singleton ("*" </> attrCacheFileName)
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

parsePackagesOrFail :: (MonadObelisk m, Foldable f) => f FilePath -> m (NE.NonEmpty CabalPackageInfo)
parsePackagesOrFail dirs' = do
  (pkgDirErrs, packageInfos') <- fmap partitionEithers $ for dirs $ \dir -> do
    flip fmap (parseCabalPackage dir) $ \case
      Just packageInfo
        | _cabalPackageInfo_buildable packageInfo -> Right packageInfo
      _ -> Left dir

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

  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find buildable packages in " <> intercalate ", " pkgDirErrs

  pure packageInfos
  where
    dirs = toList dirs'

packageInfoToNamePathMap :: Foldable f => f CabalPackageInfo -> Map Text FilePath
packageInfoToNamePathMap = Map.fromList . map (_cabalPackageInfo_packageName &&& _cabalPackageInfo_packageRoot) . toList

-- | Create ghci configuration to load the given packages
withGhciScript
  :: (MonadObelisk m, Foldable f)
  => f CabalPackageInfo -- ^ List of packages to load into ghci
  -> (FilePath -> m ()) -- ^ Action to run with the path to generated temporary .ghci
  -> m ()
withGhciScript (toList -> packageInfos) f =
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
    dotGhci = unlines
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
  -- N.B. ghci settings do NOT support escaping in any way. To minimize the likelihood that
  -- paths-with-spaces ruin our day, we first canonicalize everything, and then relativize
  -- all paths to 'pathBase'.
  selfExe <- liftIO $ canonicalizePath =<< getExecutablePath
  canonicalPathBase <- liftIO $ canonicalizePath pathBase

  (pkgFiles, pkgSrcPaths :: [NonEmpty FilePath]) <- fmap unzip $ liftIO $ for packageInfos $ \pkg -> do
    canonicalSrcDirs <- traverse canonicalizePath $ (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg
    canonicalPkgFile <- canonicalizePath $ _cabalPackageInfo_packageFile pkg
    pure (canonicalPkgFile `relativeTo` canonicalPathBase, (`relativeTo` canonicalPathBase) <$> canonicalSrcDirs)

  pure
    $  baseGhciOptions
    <> ["-F", "-pgmF", selfExe, "-optF", preprocessorIdentifier]
    <> concatMap (\p -> ["-optF", p]) pkgFiles
    <> [ "-i" <> intercalate ":" (concatMap toList pkgSrcPaths) ]

baseGhciOptions :: [String]
baseGhciOptions =
  [ "-ignore-dot-ghci"
  , "-no-user-package-db"
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
  nixShellWithoutPkgs root True False (packageInfoToNamePathMap packages) "ghc" $
    Just $ unwords $ "ghci" : ghciArgs -- TODO: Shell escape

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
    Just $ unwords $ ghcidExePath : opts -- TODO: Shell escape
  where
    opts =
      [ "-W"
      , "--outputfile=ghcid-output.txt"
      ] <> map (\x -> "--reload='" <> x <> "'") reloadFiles
        <> map (\x -> "--restart='" <> x <> "'") restartFiles
        <> testCmd
        <> ["--command='" <> unwords ("ghci" : ghciArgs) <> "'"] -- TODO: Shell escape
    testCmd = maybeToList (flip fmap mcmd $ \cmd -> "--test='" <> cmd <> "'") -- TODO: Shell escape

    adjustRoot x = if chdirToRoot then makeRelative root x else x
    reloadFiles = map adjustRoot [root </> "config"]
    restartFiles = map (adjustRoot . _cabalPackageInfo_packageFile) packages

mkGhciScriptArg :: FilePath -> [String]
mkGhciScriptArg dotGhci = ["-ghci-script", dotGhci]

getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
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
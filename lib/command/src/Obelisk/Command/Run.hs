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
import Data.Coerce (coerce)
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.Foldable (fold, for_, toList)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Semigroup (Last (..))
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
import System.Environment (getExecutablePath)
import qualified System.Info
import System.IO.Temp (withSystemTempDirectory)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (
    Severity (..),
    createProcess_,
    failWith,
    proc,
    putLog,
    readProcessAndLogStderr,
    setCwd,
    setDelegateCtlc,
    waitForProcess,
    withSpinner,
  )
import Obelisk.Command.Nix
import Obelisk.Command.Path
import Obelisk.Command.Project (findProjectAssets, nixShellWithoutPkgs, obeliskDirName, withProjectRoot)
import Obelisk.Command.Thunk (attrCacheFileName)
import Obelisk.Command.Utils (ghcidExePath)

hpackFileName :: Path Relative
hpackFileName = rel Hpack.packageConfig

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
  createDirectoryIfMissing True $ takeDirectory $ mkPath root </> rel profileBaseName
  putLog Debug [i|Storing profiled data under base name of ${root </> profileBaseName}|]
  freePort <- getFreePort
  (_, _, _, ph) <- createProcess_ "runProfExe" $ setCwd (Just root) $ setDelegateCtlc True $ proc (toFilePath $ outPath </> rel "bin" </> rel "ob-run") $
    [ show freePort
    , T.unpack assets
    , profileBaseName
    , "+RTS"
    , "-po" <> profileBaseName
    ] <> rtsFlags
      <> [ "-RTS" ]
  void $ waitForProcess ph

run :: MonadObelisk m => FilePath -> Map (Path Canonical) Interpret -> m ()
run root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  withGhciScript pkgs $ \dotGhciPath -> do
    assets <- findProjectAssets root
    putLog Debug $ "Assets impurely loaded from: " <> assets
    ghciArgs <- getGhciSessionSettings pkgs root True
    freePort <- getFreePort
    runGhcid root True (ghciArgs <> mkGhciScriptArg dotGhciPath) pkgs $ Just $ unwords
      [ "Obelisk.Run.run"
      , show freePort
      , "(Obelisk.Run.runServeAsset " ++ show assets ++ ")"
      , "Backend.backend"
      , "Frontend.frontend"
      ]

runRepl :: MonadObelisk m => FilePath -> Map (Path Canonical) Interpret -> m ()
runRepl root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs (mkPath ".") True
  withGhciScript pkgs $ \dotGhciPath ->
    runGhciRepl root pkgs (ghciArgs <> mkGhciScriptArg dotGhciPath)

runWatch :: MonadObelisk m => FilePath -> Map (Path Canonical) Interpret -> m ()
runWatch root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  ghciArgs <- getGhciSessionSettings pkgs root True
  withGhciScript pkgs $ \dotGhciPath ->
    runGhcid root True (ghciArgs <> mkGhciScriptArg dotGhciPath) pkgs Nothing

exportGhciConfig :: MonadObelisk m => Bool -> FilePath -> Map (Path Canonical) Interpret -> m [String]
exportGhciConfig useRelativePaths root interpretPaths = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  getGhciSessionSettings pkgs (rel ".") useRelativePaths

nixShellForInterpretPaths :: MonadObelisk m => Bool -> String -> FilePath -> Map (Path Canonical) Interpret -> Maybe String -> m ()
nixShellForInterpretPaths isPure shell root interpretPaths cmd = do
  pkgs <- getParsedLocalPkgs root interpretPaths
  nixShellWithoutPkgs root isPure False (packageInfoToNamePathMap pkgs) shell cmd

-- | Like 'getLocalPkgs' but also parses them and fails if any of them can't be parsed.
getParsedLocalPkgs :: MonadObelisk m => FilePath -> Map (Path Canonical) Interpret -> m (NonEmpty CabalPackageInfo)
getParsedLocalPkgs root interpretPaths = parsePackagesOrFail =<< getLocalPkgs root interpretPaths

-- | Relative paths to local packages of an obelisk project.
--
-- These are a combination of the obelisk predefined local packages,
-- and any packages that the user has set with the @packages@ argument
-- to the Nix @project@ function.
getLocalPkgs :: forall m. MonadObelisk m => FilePath -> Map (Path Canonical) Interpret -> m (Set (Path Canonical))
getLocalPkgs root interpretPaths = do
  let pathTree :: PathTree Interpret = coerce $ foldMap (\(p, x) -> pathToTree (Last x) p) $ Map.toList interpretPaths
  putLog Debug $ [i|Finding packages with root ${root} and interpret paths:|] <> "\n" <> drawPathTree textInterpret pathTree

  -- We do not want to find packages that are embedded inside other obelisk projects, unless that
  -- obelisk project is our own.
  canonicalRoot <- canonicalizePath root
  obeliskPackageExclusions <- findFiles'
    (\_ p -> if takeFileName p == rel obeliskDirName && p /= canonicalRoot
      then liftIO (doesDirectoryExist p) <&> \case
            True -> FindMatch_Match $ Set.singleton p
            False -> FindMatch_SearchTree
      else pure FindMatch_SearchTree
    )
    root

  let
    findRoots = Map.keysSet $ Map.filter (== Interpret_Interpret) interpretPaths
    exclusions = Map.keysSet $ Map.filter (== Interpret_NoInterpret) interpretPaths
      <> Map.fromSet (const Interpret_NoInterpret) obeliskPackageExclusions -- Add these last because we want left side to override

  fmap fold $ for (toList findRoots) $ \interpretPathRoot -> do
    findFiles'
        (\_ p -> pure $ case () of
          _ | p `Set.member` exclusions || attrCacheFileName `elem` splitDirectories p -> FindMatch_SkipTree
            | takeExtension p == ".cabal" || takeFileName p == hpackFileName -> FindMatch_Match $ Set.singleton p
            | otherwise -> FindMatch_SearchTree
        )
        interpretPathRoot

data GuessPackageFileError = GuessPackageFileError_Ambiguous [FilePath] | GuessPackageFileError_NotFound
  deriving (Eq, Ord, Show)
instance Exception GuessPackageFileError

newtype HPackFilePath = HPackFilePath { unHPackFilePath :: FilePath } deriving (Eq, Ord, Show)
newtype CabalFilePath = CabalFilePath { unCabalFilePath :: FilePath } deriving (Eq, Ord, Show)

-- | Given a directory, try to guess what the appropriate @.cabal@ or @package.yaml@ file is for the package.
guessCabalPackageFile
  :: (MonadIO m, IsPath path)
  => path -- ^ Directory or path to search for cabal package
  -> m (Either GuessPackageFileError (Either CabalFilePath HPackFilePath))
guessCabalPackageFile (toFilePath -> pkg) = do
  liftIO (doesDirectoryExist pkg) >>= \case
    False -> case cabalOrHpackFile pkg of
      (Just hpack@(Right _)) -> pure $ Right hpack
      (Just cabal@(Left (CabalFilePath cabalFilePath))) -> do
        -- If the cabal file has a sibling hpack file, we use that instead
        -- since running hpack often generates a sibling cabal file
        let possibleHpackSibling = takeDirectory $ mkPath cabalFilePath </> hpackFileName
        hasHpackSibling <- liftIO $ doesFileExist possibleHpackSibling
        pure $ Right $ if hasHpackSibling then Right (HPackFilePath $ toFilePath possibleHpackSibling) else cabal
      Nothing -> pure $ Left GuessPackageFileError_NotFound
    True -> do
      candidates <- liftIO $
            filterM (doesFileExist . either unCabalFilePath unHPackFilePath)
        =<< mapMaybe (cabalOrHpackFile . toFilePath . (pkg </>)) <$> listDirectory pkg
      pure $ case partitionEithers candidates of
        ([hpack], _) -> Right $ Left hpack
        ([], [cabal]) -> Right $ Right cabal
        ([], []) -> Left GuessPackageFileError_NotFound
        (hpacks, cabals) -> Left $ GuessPackageFileError_Ambiguous $ coerce hpacks <> coerce cabals

cabalOrHpackFile :: FilePath -> Maybe (Either CabalFilePath HPackFilePath)
cabalOrHpackFile = \case
  x | takeExtension x == ".cabal" -> Just (Left $ CabalFilePath x)
    | takeFileName x == hpackFileName -> Just (Right $ HPackFilePath x)
    | otherwise -> Nothing

-- | Parses the cabal package in a given directory.
-- This automatically figures out which .cabal file or package.yaml (hpack) file to use in the given directory.
parseCabalPackage
  :: (MonadObelisk m, IsPath path)
  => path
  -> m (Maybe CabalPackageInfo)
parseCabalPackage dir = parseCabalPackage' dir >>= \case
  Left err -> Nothing <$ putLog Error err
  Right (warnings, pkgInfo) -> do
    for_ warnings $ putLog Warning . T.pack . show
    pure $ Just pkgInfo

-- | Like 'parseCabalPackage' but returns errors and warnings directly so as to avoid 'MonadObelisk'.
parseCabalPackage'
  :: (MonadIO m, IsPath path)
  => path -- ^ Package directory
  -> m (Either T.Text ([Dist.PWarning], CabalPackageInfo))
parseCabalPackage' (toFilePath -> pkg) = runExceptT $ do
  (cabalContents, packageFile, packageName) <- guessCabalPackageFile pkg >>= \case
    Left GuessPackageFileError_NotFound -> throwError [i|No .cabal or package.yaml file found in ${pkg}|]
    Left (GuessPackageFileError_Ambiguous _) -> throwError [i|Unable to determine which .cabal file to use in ${pkg}|]
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
        , _cabalPackageInfo_packageRoot = unPath $ takeDirectory (mkPath packageFile)
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
      throwError $ [i|Failed to parse ${packageFile}:|] <> "\n" <> T.pack (unlines (map show errors))

parsePackagesOrFail :: (MonadObelisk m, Foldable f, IsPath path) => f path -> m (NE.NonEmpty CabalPackageInfo)
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
      "No valid, buildable packages found" <> (if null dirs then "" else " in " <> intercalate ", " (map toFilePath dirs))
    Just xs -> pure xs

  unless (null pkgDirErrs) $
    putLog Warning $ T.pack $ "Failed to find buildable packages in " <> intercalate ", " (map toFilePath pkgDirErrs)

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
    let dotGhciPath = toFilePath $ fp </> rel ".ghci"
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
  :: (MonadObelisk m, Foldable f, IsPath path)
  => f CabalPackageInfo -- ^ List of packages to load into ghci
  -> path -- ^ All paths will be relative to this path
  -> Bool -- ^ Use relative paths
  -> m [String]
getGhciSessionSettings (toList -> packageInfos) pathBase useRelativePaths = do
  -- N.B. ghci settings do NOT support escaping in any way. To minimize the likelihood that
  -- paths-with-spaces ruin our day, we first canonicalize everything, and then relativize
  -- all paths to 'pathBase'.
  selfExe <- canonicalizePath =<< liftIO getExecutablePath
  canonicalPathBase <- canonicalizePath pathBase

  (pkgFiles, pkgSrcPaths :: [NonEmpty FilePath]) <- fmap unzip $ liftIO $ for packageInfos $ \pkg -> do
    canonicalSrcDirs <- traverse canonicalizePath $ (_cabalPackageInfo_packageRoot pkg </>) <$> _cabalPackageInfo_sourceDirs pkg
    canonicalPkgFile <- canonicalizePath $ _cabalPackageInfo_packageFile pkg
    pure (canonicalPkgFile `relativeTo'` canonicalPathBase, (`relativeTo'` canonicalPathBase) <$> canonicalSrcDirs)

  pure
    $  baseGhciOptions
    <> ["-F", "-pgmF", toFilePath selfExe, "-optF", preprocessorIdentifier]
    <> concatMap (\p -> ["-optF", p]) pkgFiles
    <> [ "-i" <> intercalate ":" (concatMap toList pkgSrcPaths) ]
  where
    relativeTo' :: Path Canonical -> Path Canonical -> FilePath
    relativeTo' a b = if useRelativePaths then toFilePath $ relativeTo a b else toFilePath a

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
    reloadFiles = map adjustRoot [toFilePath $ root </> rel "config"]
    restartFiles = map (adjustRoot . _cabalPackageInfo_packageFile) packages

mkGhciScriptArg :: IsPath path => path -> [String]
mkGhciScriptArg dotGhci = ["-ghci-script", toFilePath dotGhci]

getFreePort :: MonadIO m => m PortNumber
getFreePort = liftIO $ withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock



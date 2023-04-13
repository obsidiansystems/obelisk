{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PackageImports #-}

module Obelisk.Command.Project
  ( InitSource (..)
  , findProjectObeliskCommand
  , findProjectRoot
  , findProjectAssets
  , initProject
  , nixShellRunConfig
  , nixShellRunProc
  , nixShellWithHoogle
  , nixShellWithoutPkgs
  , mkObNixShellProc
  , obeliskDirName
  , toImplDir
  , toObeliskDir
  , withProjectRoot
  , bashEscape
  , shEscape
  , getHaskellManifestProjectPath
  , AssetSource(..)
  , describeImpureAssetSource
  , watchStaticFilesDerivation
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVarMasked)
import Control.Lens ((.~), (?~), (<&>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log
import Control.Monad.State
import qualified Data.Aeson as Json
import qualified Data.ByteString.UTF8 as BSU
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Default (def)
import qualified Data.Foldable as F (toList)
import Data.Function ((&), on)
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.List as L
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Traversable (for)
import "nix-thunk" Nix.Thunk
import Reflex
import Reflex.FSNotify
import Reflex.Host.Headless
import System.Directory
import System.Environment (lookupEnv, getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath
import System.FSNotify (defaultConfig, eventPath, WatchConfig(..))
import qualified System.Info as SysInfo
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)
import System.PosixCompat.Files
import System.PosixCompat.Types
import System.PosixCompat.User
import qualified System.Process as Proc
import Text.ShellEscape (sh, bash, bytes)
import qualified Data.Map as Map

import GitHub.Data.GitData (Branch)
import GitHub.Data.Name (Name)

import Obelisk.App (MonadObelisk, runObelisk, getObelisk, wrapNixThunkError)
import Obelisk.Command.Nix
import Cli.Extras
import Obelisk.Command.Utils (nixBuildExePath, nixExePath, toNixPath, cp, nixShellPath, lnPath)

--TODO: Make this module resilient to random exceptions

--TODO: Don't hardcode this
-- | Source for the Obelisk project
obeliskSource :: ThunkSource
obeliskSource = obeliskSourceWithBranch "master"

-- | Source for obelisk developer targeting a specific obelisk branch
obeliskSourceWithBranch :: Name Branch -> ThunkSource
obeliskSourceWithBranch branch = ThunkSource_GitHub $ GitHubSource
  { _gitHubSource_owner = "obsidiansystems"
  , _gitHubSource_repo = "obelisk"
  , _gitHubSource_branch = Just branch
  , _gitHubSource_private = False
  }

data InitSource
  = InitSource_Default
  | InitSource_Branch (Name Branch)
  | InitSource_Symlink FilePath
  deriving Show

obeliskDirName :: FilePath
obeliskDirName = ".obelisk"

-- | Path to obelisk directory in given path
toObeliskDir :: FilePath -> FilePath
toObeliskDir p = p </> obeliskDirName

-- | Path to impl file in given path
toImplDir :: FilePath -> FilePath
toImplDir p = toObeliskDir p </> "impl"

-- | Create a new project rooted in the current directory
initProject :: forall m. MonadObelisk m => InitSource -> Bool -> m ()
initProject source force = withSystemTempDirectory "ob-init" $ \tmpDir -> do
  let implDir = toImplDir tmpDir
      obDir   = toObeliskDir tmpDir
  liftIO (listDirectory ".") >>= \case
    [] -> pure ()
    _ | force -> putLog Warning "Initializing in non-empty directory"
      | otherwise -> failWith "ob init requires an empty directory. Use the flag --force to init anyway, potentially overwriting files."
  skeleton <- withSpinner "Setting up obelisk" $ do
    liftIO $ createDirectory obDir
    -- Clone the git source and repack it with the init source obelisk
    -- The purpose of this is to ensure we use the correct thunk spec.
    let cloneAndRepack :: ThunkSource -> m ()
        cloneAndRepack src = do
          putLog Debug $ "Cloning obelisk into " <> T.pack implDir <> " and repacking using itself"
          commit <- wrapNixThunkError $ getLatestRev src
          wrapNixThunkError $ gitCloneForThunkUnpack (thunkSourceToGitSource src) (_thunkRev_commit commit) implDir
          callHandoffOb implDir ["thunk", "pack", implDir]
    case source of
      InitSource_Default -> cloneAndRepack obeliskSource
      InitSource_Branch branch -> cloneAndRepack $ obeliskSourceWithBranch branch
      InitSource_Symlink path -> do
        let symlinkPath = if isAbsolute path
              then path
              else ".." </> path
        liftIO $ createSymbolicLink symlinkPath implDir
    _ <- wrapNixThunkError $ nixBuildAttrWithCache implDir "command"
    skel <- wrapNixThunkError $ nixBuildAttrWithCache implDir "skeleton" --TODO: I don't think there's actually any reason to cache this

    callProcessAndLogOutput (Notice, Error) $
      proc cp
        [ "-r"
        , "--preserve=links"
        , obDir
        , toObeliskDir "."
        ]
    pure skel

  withSpinner "Copying project skeleton" $ do
    callProcessAndLogOutput (Notice, Error) $
      proc cp
        [ "-r"
        , "--no-preserve=mode"
        , "-T"
        , skeleton </> "."
        , "."
        ]
  liftIO $ do
    let configDir = "config"
    createDirectoryIfMissing False configDir
    mapM_ (createDirectoryIfMissing False . (configDir </>)) ["backend", "common", "frontend"]
  putLog Notice $ T.intercalate "\n"
    [ "An obelisk project has been successfully initialized. Next steps:"
    , "  'ob run': Start a development server"
    , "  'ob watch': Watch for changes without starting a server"
    , "  'ob repl': Load your project into GHCi"
    ]

callHandoffOb
  :: MonadObelisk m
  => FilePath -- ^ Directory of the obelisk we want to handoff to
  -> [String] -- ^ Arguments to pass to ob
  -> m ()
callHandoffOb dir args = do
  obeliskCommandPkg <- nixCmd $ NixCmd_Build $ def
    & nixBuildConfig_outLink .~ OutLink_None
    & nixCmdConfig_target .~ Target
      { _target_path = Just dir
      , _target_attr = Just "command"
      , _target_expr = Nothing
      }
  let impl = obeliskCommandPkg </> "bin" </> "ob"
  -- Invoke the real implementation, using --no-handoff to prevent infinite recursion
  putLog Debug $ "Running '" <> T.pack (unwords args) <> "' with " <> T.pack impl
  callProcessAndLogOutput (Debug, Warning) (proc impl ("--no-handoff" : args))

--TODO: Allow the user to ignore our security concerns
-- | Find the Obelisk implementation for the project at the given path
findProjectObeliskCommand :: MonadObelisk m => FilePath -> m (Maybe FilePath)
findProjectObeliskCommand target = do
  myUid <- liftIO getRealUserID
  processUmask <- liftIO getUmask
  targetStat <- liftIO $ getFileStatus target
  (result, insecurePaths) <- flip runStateT [] $ walkToProjectRoot target targetStat processUmask myUid >>= \case
    Nothing -> pure Nothing
    Just projectRoot -> liftIO (doesDirectoryExist $ toImplDir projectRoot) >>= \case
      False -> do
        putLog Warning $ "Found obelisk directory in " <> T.pack projectRoot <> " but the implementation (impl) file is missing"
        pure Nothing
      True -> do
        walkToImplDir projectRoot myUid processUmask -- For security check
        return $ Just projectRoot
  case (result, insecurePaths) of
    (Just projDir, []) -> do
      obeliskCommandPkg <- wrapNixThunkError $ nixBuildAttrWithCache (toImplDir projDir) "command"
      return $ Just $ obeliskCommandPkg </> "bin" </> "ob"
    (Nothing, _) -> return Nothing
    (Just projDir, _) -> do
      putLog Error $ T.unlines
        [ "Error: Found a project at " <> T.pack (normalise projDir) <> ", but had to traverse one or more insecure directories to get there:"
        , T.unlines $ fmap (T.pack . normalise) insecurePaths
        , "Please ensure that all of these directories are owned by you, not world-writable, and no more group-writable than permitted by your umask."
        ]
      return Nothing

-- | Get the umask for the Obelisk process.
--
-- Because of
-- http://man7.org/linux/man-pages/man2/umask.2.html#NOTES we have to set the
-- umask to read it. We are using 'withMVarMasked' to guarantee that setting and
-- reading isn't interrupted by any exception or interleaved with another thread.
getUmask :: IO FileMode
getUmask = withMVarMasked globalUmaskLock $ \() -> do
  initialMask <- setFileCreationMask safeUmask
  void (setFileCreationMask initialMask)
  pure initialMask
  where
    safeUmask :: FileMode
    safeUmask = CMode 0o22

{-# NOINLINE globalUmaskLock #-}
globalUmaskLock :: MVar ()
globalUmaskLock = unsafePerformIO (newMVar ())

-- | Get the FilePath to the containing project directory, if there is one
findProjectRoot :: MonadObelisk m => FilePath -> m (Maybe FilePath)
findProjectRoot target = do
  myUid <- liftIO getRealUserID
  targetStat <- liftIO $ getFileStatus target
  umask <- liftIO getUmask
  (result, _) <- liftIO $ runStateT (walkToProjectRoot target targetStat umask myUid) []
  return $ makeRelative "." <$> result

withProjectRoot :: MonadObelisk m => FilePath -> (FilePath -> m a) -> m a
withProjectRoot target f = findProjectRoot target >>= \case
  Nothing -> failWith "Must be used inside of an Obelisk project"
  Just root -> f root

-- | Walk from the current directory to the containing project's root directory,
-- if there is one, accumulating potentially insecure directories that were
-- traversed in the process.  Return the project root directory, if found.
walkToProjectRoot
  :: (MonadState [FilePath] m, MonadIO m)
  => FilePath -> FileStatus -> FileMode -> UserID -> m (Maybe FilePath)
walkToProjectRoot this thisStat desiredUmask myUid = liftIO (doesDirectoryExist this) >>= \case
  -- It's not a directory, so it can't be a project
  False -> do
    let dir = takeDirectory this
    dirStat <- liftIO $ getFileStatus dir
    walkToProjectRoot dir dirStat desiredUmask myUid
  True -> do
    when (not $ isWellOwnedAndWellPermissioned thisStat myUid desiredUmask) $ modify (this:)
    liftIO (doesDirectoryExist $ toObeliskDir this) >>= \case
      True -> return $ Just this
      False -> do
        let next = this </> ".." -- Use ".." instead of chopping off path segments, so that if the current directory is moved during the traversal, the traversal stays consistent
        nextStat <- liftIO $ getFileStatus next
        let fileIdentity fs = (deviceID fs, fileID fs)
            isSameFileAs = (==) `on` fileIdentity
        if thisStat `isSameFileAs` nextStat
          then return Nothing -- Found a cycle; probably hit root directory
          else walkToProjectRoot next nextStat desiredUmask myUid

-- | Walk from the given project root directory to its Obelisk implementation
-- directory, accumulating potentially insecure directories that were traversed
-- in the process.
walkToImplDir :: (MonadState [FilePath] m, MonadIO m) => FilePath -> UserID -> FileMode -> m ()
walkToImplDir projectRoot myUid umask = do
  let obDir = toObeliskDir projectRoot
  obDirStat <- liftIO $ getFileStatus obDir
  when (not $ isWellOwnedAndWellPermissioned obDirStat myUid umask) $ modify (obDir:)
  let implThunk = obDir </> "impl"
  implThunkStat <- liftIO $ getFileStatus implThunk
  when (not $ isWellOwnedAndWellPermissioned implThunkStat myUid umask) $ modify (implThunk:)

-- | Check to see if directory is writable by a user whose User ID matches the
-- second argument provided, and if the fact that other people can write to that
-- directory is in accordance with the umask of the system, passed as the third
-- argument.
isWellOwnedAndWellPermissioned :: FileStatus -> UserID -> FileMode -> Bool
isWellOwnedAndWellPermissioned s uid umask = isOwnedBy s uid && filePermissionIsSafe s umask

isOwnedBy :: FileStatus -> UserID -> Bool
isOwnedBy s uid = fileOwner s == uid

-- | Check to see if a directory respect the umask, but check explicitly that
-- it's not world writable in any case.
filePermissionIsSafe :: FileStatus -> FileMode -> Bool
filePermissionIsSafe s umask = not fileWorldWritable && fileGroupWritable <= umaskGroupWritable
  where
    fileWorldWritable = fileMode s .&. 0o002 == 0o002
    fileGroupWritable = fileMode s .&. 0o020 == 0o020
    umaskGroupWritable = umask .&. 0o020 == 0

nixShellRunConfig :: MonadObelisk m => FilePath -> Bool -> Maybe String -> m NixShellConfig
nixShellRunConfig root isPure command = do
  nixpkgsPath <- fmap T.strip $ readProcessAndLogStderr Debug $ setCwd (Just root) $
    proc nixExePath ["eval", "--impure", "--expr", "(import ./. {}).pkgs.path"]
  nixRemote <- liftIO $ lookupEnv "NIX_REMOTE"
  environment <- liftIO getEnvironment
  let environmentConf = [( "NIX_PATH", "nixpkgs=" ++ BSU.toString (encodeUtf8 nixpkgsPath))]
                        <> Prelude.concatMap
                          (\c -> do
                            [c | "bindir" `L.isSuffixOf` fst c])
                            environment
      environmentExport = flip concatMap environmentConf $ \x -> ["export", fst x <> "=" <> snd x, ";"]
  pure $ def
    & nixShellConfig_pure .~ isPure
    & nixShellConfig_common . nixCmdConfig_target .~ (def & target_path .~ Nothing)
    & nixShellConfig_run .~ (command <&> \cs -> unwords $ concat
      [ maybe [] (\v -> ["export", BSU.toString . bytes . bash $ "NIX_REMOTE=" <> encodeUtf8 (T.pack v), ";"]) nixRemote
      , environmentExport
      , [cs]
      ])
    & nixShellConfig_env .~ environmentConf
-- | Escape using ANSI C-style quotes @$''@
-- This does not work with all shells! Ideally, we would control exactly which shell is used,
-- down to its sourced configuration, throughout the obelisk environment. At this time, this
-- is not feasible.
bashEscape :: String -> String
bashEscape = BSU.toString . bytes . bash . BSU.fromString

-- | Escape using Bourne style shell escaping
-- This is not as robust, but is necessary if we are passing to a shell we don't control.
-- The most prominent issue is that 'System.Process' executes shell commands by invoking
-- @\/bin\/sh@ instead of something configurable. While we can avoid this by specifying a shell manually,
-- we cannot guarantee that our dependencies do the same. In particular, ghcid invokes its
-- subcommands that way.
shEscape :: String -> String
shEscape = BSU.toString . bytes . sh . BSU.fromString

nixShellRunProc :: NixShellConfig -> ProcessSpec
nixShellRunProc cfg = let p = setDelegateCtlc True $ proc nixShellPath $ runNixShellConfig cfg
                      in setEnvOverride (\oldMap -> (Map.union (Map.fromList (_nixShellConfig_env cfg)) oldMap)) p
mkObNixShellProc
  :: MonadObelisk m
  => FilePath -- ^ Path to project root
  -> Bool -- ^ Should this be a pure shell?
  -> Bool -- ^ Should we chdir to the package root in the shell?
  -> Map Text FilePath -- ^ Package names mapped to their paths
  -> String -- ^ Shell attribute to use (e.g. @"ghc"@, @"ghcjs"@, etc.)
  -> Maybe String -- ^ If 'Just' run the given command; otherwise just open the interactive shell
  -> m ProcessSpec
mkObNixShellProc root isPure chdirToRoot packageNamesAndPaths shellAttr command = do
  packageNamesAndAbsPaths <- liftIO $ for packageNamesAndPaths makeAbsolute
  defShellConfig <- nixShellRunConfig root isPure command
  let setCwd_ = if chdirToRoot then setCwd (Just root) else id
  pure $ setCwd_ $ nixShellRunProc $ defShellConfig
    & nixShellConfig_common . nixCmdConfig_target . target_expr ?~
        "{root, pkgs, shell}: (import root {}).combinedShell"
   {-     passthru.__unstable__.self.extend (_: _: {\
          \shellPackages = builtins.fromJSON pkgs;\
        \})).project.shells.${shell}"
    -}
    & nixShellConfig_common . nixCmdConfig_args .~
        [ rawArg "root" $ toNixPath $ if chdirToRoot then "." else root
        , strArg "pkgs" (T.unpack $ decodeUtf8 $ BSL.toStrict $ Json.encode packageNamesAndAbsPaths)
        , strArg "shell" shellAttr
        ]

nixShellWithoutPkgs
  :: MonadObelisk m
  => FilePath -- ^ Path to project root
  -> Bool -- ^ Should this be a pure shell?
  -> Bool -- ^ Should we chdir to the package root in the shell?
  -> Map Text FilePath -- ^ Package names mapped to their paths
  -> String -- ^ Shell attribute to use (e.g. @"ghc"@, @"ghcjs"@, etc.)
  -> Maybe String -- ^ If 'Just' run the given command; otherwise just open the interactive shell
  -> m ()
nixShellWithoutPkgs root isPure chdirToRoot packageNamesAndPaths shellAttr command = do
  runProcess_ =<< mkObNixShellProc root isPure chdirToRoot packageNamesAndPaths shellAttr command

nixShellWithHoogle :: MonadObelisk m => FilePath -> Bool -> String -> Maybe String -> m ()
nixShellWithHoogle root isPure shell' command = do
  defShellConfig <- nixShellRunConfig root isPure command
  runProcess_ $ setCwd (Just root) $ nixShellRunProc $ defShellConfig
    & nixShellConfig_common . nixCmdConfig_target . target_expr ?~
        "{shell}: (import ./. {}).hoogleShell"
    & nixShellConfig_common . nixCmdConfig_args .~ [ strArg "shell" shell' ]

-- | Describes the provenance of static assets (i.e., are they the result of a derivation
-- that was built, or just a folder full of files.
data AssetSource = AssetSource_Derivation
                 | AssetSource_Files
  deriving (Eq)

-- | Some log messages to make it easier to tell where static files are coming from
describeImpureAssetSource :: AssetSource -> Text -> Text
describeImpureAssetSource src path = case src of
  AssetSource_Files -> "Assets impurely loaded from: " <> path
  AssetSource_Derivation -> "Assets derivation built and impurely loaded from: " <> path

-- | Determine where the static files of a project are and whether they're plain files or a derivation.
-- If they are a derivation, that derivation will be built.
findProjectAssets :: MonadObelisk m => FilePath -> m (AssetSource, Text)
findProjectAssets root = do
  isDerivation <- readProcessAndLogStderr Debug $ setCwd (Just root) $
    proc nixExePath
      [ "eval"
      , "--impure"
      , "--expr"
      , "(let a = import ./. {}; in toString (a.pkgs.lib.isDerivation a.passthru.staticFilesImpure))"
      , "--raw"
      -- `--expr` and `--impure` are a side-effect of a newer nix version
      -- `nix eval` is no longer the same as 2.3
      -- `--raw` is not available with old nix-instantiate. It drops quotation
      -- marks and trailing newline, so is very convenient for shelling out.
      ]
  -- Check whether the impure static files are a derivation (and so must be built)
  if isDerivation == "1"
    then do
      _ <- buildStaticFilesDerivationAndSymlink
        (readProcessAndLogStderr Debug)
        root
      pure (AssetSource_Derivation, T.pack $ root </> "static.out")
    else fmap (AssetSource_Files,) $ do
      path <- readProcessAndLogStderr Debug $ setCwd (Just root) $
        proc nixExePath ["eval", "-f", ".", "passthru.staticFilesImpure", "--raw"]
      _ <- readProcessAndLogStderr Debug $ setCwd (Just root) $
        proc lnPath ["-sfT", T.unpack path, "./static.out"]
      pure path

-- | Get the nix store path to the generated static asset manifest module (e.g., "obelisk-generated-static")
getHaskellManifestProjectPath :: MonadObelisk m => FilePath -> m Text
getHaskellManifestProjectPath root = fmap T.strip $ readProcessAndLogStderr Debug $ setCwd (Just root) $
  proc nixBuildExePath
    [ "--no-out-link"
    , "-E"
    , "(let a = import ./. {}; in a.passthru.processedStatic.haskellManifest)"
    ]

-- | Watch the common, backend, frontend, and static directories for file
-- changes and check whether those file changes cause changes in the static
-- files nix derivation. If so, rebuild it.
watchStaticFilesDerivation
  :: (MonadIO m, MonadObelisk m)
  => FilePath
  -> m ()
watchStaticFilesDerivation root = do
  ob <- getObelisk
  liftIO $ runHeadlessApp $ do
    pb <- getPostBuild
    -- TODO: Instead of filtering like this, we should figure out what the
    -- derivation actually relies on, or at least use the gitignore
    let filterEvents x =
          let fn = takeFileName x
              dirs = Set.fromList $ splitDirectories x
              ignoredFilenames = Set.fromList
                [ "4913" -- Vim temporary file
                ]
              ignoredExtensions = Set.fromList
                [ ".hi"
                , ".o"
                , ".swo"
                , ".swp"
                ]
          in not $
              fn `Set.member` ignoredFilenames ||
              takeExtension fn `Set.member` ignoredExtensions
        cfg = defaultConfig
          -- On macOS, use the polling backend due to
          -- https://github.com/luite/hfsevents/issues/13
          { confUsePolling = SysInfo.os == "darwin"
          , confPollInterval = 250000
          }
        watch' pkg = fmap (:[]) <$> watchDirectoryTree cfg (root </> pkg <$ pb) (filterEvents . eventPath)
    rebuild <- batchOccurrences 0.25 =<< mergeWith (<>) <$> mapM watch'
      [ "frontend"
      , "backend"
      , "common"
      , "static"
      ]
    performEvent_
      $ liftIO
      . runObelisk ob
      . putLog Debug
      . ("Regenerating static.out due to file changes: "<>)
      . T.intercalate ", "
      . Set.toList
      . Set.fromList
      . fmap (T.pack . eventPath)
      . concat
      . F.toList
      <$> rebuild
    void $ flip throttleBatchWithLag rebuild $ \e ->
      performEvent $ ffor e $ \_ -> liftIO $ runObelisk ob $ do
        putLog Notice "Static assets being built..."
        buildStaticCatchErrors >>= \case
          Nothing -> pure ()
          Just n -> do
            putLog Notice $ "Static assets built and symlinked to static.out"
            putLog Debug $ "Generated static asset nix path: " <> n
    pure never
  where
    handleBuildFailure
      :: MonadObelisk m
      => (ExitCode, String, String)
      -> m (Maybe Text)
    handleBuildFailure (ex, out, err) = case ex of
      ExitSuccess ->
        let out' = T.strip $ T.pack out
        in pure $ if T.null out' then Nothing else Just out'
      _ -> do
        putLog Error $
          ("Static assets build failed: " <>) $
            T.unlines $ reverse $ take 20 $ reverse $ T.lines $ T.pack err
        pure Nothing
    buildStaticCatchErrors :: MonadObelisk m => m (Maybe Text)
    buildStaticCatchErrors = handleBuildFailure =<<
      buildStaticFilesDerivationAndSymlink
        readCreateProcessWithExitCode
        root

buildStaticFilesDerivationAndSymlink
  :: MonadObelisk m
  => (ProcessSpec -> m a)
  -> FilePath
  -> m a
buildStaticFilesDerivationAndSymlink f root = f $
  setCwd (Just root) $ ProcessSpec
    { _processSpec_createProcess = Proc.proc
        nixBuildExePath
        [ "-o", "static.out"
        , "-E", "(import ./. {}).passthru.staticFilesImpure"
        ]
    , _processSpec_overrideEnv = Nothing
    }

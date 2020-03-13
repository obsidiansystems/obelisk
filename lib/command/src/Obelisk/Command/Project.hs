{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Project
  ( InitSource (..)
  , findProjectObeliskCommand
  , findProjectRoot
  , initProject
  , nixShellRunConfig
  , nixShellRunProc
  , nixShellWithHoogle
  , nixShellWithPkgs
  , obeliskDirName
  , projectShell
  , toImplDir
  , toNixPath
  , toObeliskDir
  , withProjectRoot
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVarMasked)
import Control.Lens ((.~), (?~), (<&>))
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State
import qualified Data.Aeson as Json
import Data.Bits
import qualified Data.ByteString.Lazy as BSL
import Data.Default (def)
import Data.Function ((&), on)
import Data.List (isInfixOf)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Traversable (for)
import System.Directory
import System.Environment (lookupEnv)
import System.FilePath
import System.IO.Temp
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (FileStatus, FileMode, CMode (..), UserID, deviceID, fileID, fileMode, fileOwner, getFileStatus, getRealUserID)
import System.Posix.Files
import System.Process (waitForProcess)

import GitHub.Data.GitData (Branch)
import GitHub.Data.Name (Name)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Nix
import Obelisk.Command.Thunk
import Obelisk.Command.Utils (nixExePath)

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
initProject :: MonadObelisk m => InitSource -> Bool -> m ()
initProject source force = withSystemTempDirectory "ob-init" $ \tmpDir -> do
  let implDir = toImplDir tmpDir
      obDir   = toObeliskDir tmpDir
  liftIO (listDirectory ".") >>= \case
    [] -> pure ()
    _ | force -> putLog Warning "Initializing in non-empty directory"
      | otherwise -> failWith "ob init requires an empty directory. Use the flag --force to init anyway, potentially overwriting files."
  skeleton <- withSpinner "Setting up obelisk" $ do
    liftIO $ createDirectory obDir
    case source of
      InitSource_Default -> createThunkWithLatest implDir obeliskSource
      InitSource_Branch branch -> createThunkWithLatest implDir $ obeliskSourceWithBranch branch
      InitSource_Symlink path -> do
        let symlinkPath = if isAbsolute path
              then path
              else ".." </> path
        liftIO $ createSymbolicLink symlinkPath implDir
    _ <- nixBuildAttrWithCache implDir "command"
    --TODO: We should probably handoff to the impl here
    skel <- nixBuildAttrWithCache implDir "skeleton" --TODO: I don't think there's actually any reason to cache this

    callProcessAndLogOutput (Notice, Error) $
      proc "cp"
        [ "-r"
        , "--preserve=links"
        , obDir
        , toObeliskDir "."
        ]
    pure skel

  withSpinner "Copying project skeleton" $ do
    callProcessAndLogOutput (Notice, Error) $
      proc "cp"
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
      obeliskCommandPkg <- nixBuildAttrWithCache (toImplDir projDir) "command"
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

-- | Nix syntax requires relative paths to be prefixed by @./@ or
-- @../@. This will make a 'FilePath' that can be embedded in a Nix
-- expression.
toNixPath :: FilePath -> FilePath
toNixPath root | "/" `isInfixOf` root = root
               | otherwise = "./" <> root

nixShellRunConfig :: MonadObelisk m => FilePath -> Bool -> Maybe String -> m NixShellConfig
nixShellRunConfig root isPure command = do
  nixpkgsPath <- fmap T.strip $ readProcessAndLogStderr Debug $ setCwd (Just root) $
    proc nixExePath ["eval", "(import .obelisk/impl {}).nixpkgs.path"]
  nixRemote <- liftIO $ lookupEnv "NIX_REMOTE"
  pure $ def
    & nixShellConfig_pure .~ isPure
    & nixShellConfig_common . nixCmdConfig_target .~ (def & target_path .~ Nothing)
    & nixShellConfig_run .~ (command <&> \c -> mconcat
      [ "export NIX_PATH=nixpkgs=", T.unpack nixpkgsPath, "; "
      , maybe "" (\v -> "export NIX_REMOTE=" <> v <> "; ") nixRemote
      , c
      ])

nixShellRunProc :: NixShellConfig -> ProcessSpec
nixShellRunProc cfg = setDelegateCtlc True $ proc "nix-shell" $ runNixShellConfig cfg

nixShellWithPkgs :: MonadObelisk m => FilePath -> Bool -> Bool -> Map Text FilePath -> Maybe String -> m ()
nixShellWithPkgs root isPure chdirToRoot packageNamesAndPaths command = do
  packageNamesAndAbsPaths <- liftIO $ for packageNamesAndPaths makeAbsolute
  defShellConfig <- nixShellRunConfig root isPure command
  let setCwd_ = if chdirToRoot then setCwd (Just root) else id
  (_, _, _, ph) <- createProcess_ "nixShellWithPkgs" $ setCwd_ $ nixShellRunProc $ defShellConfig
    & nixShellConfig_common . nixCmdConfig_target . target_expr ?~
        "{root, pkgs}: ((import root {}).passthru.__unstable__.self.extend (_: _: {\
          \shellPackages = builtins.fromJSON pkgs;\
        \})).project.shells.ghc"
    & nixShellConfig_common . nixCmdConfig_args .~
        [ rawArg "root" $ toNixPath $ if chdirToRoot then "." else root
        , strArg "pkgs" (T.unpack $ decodeUtf8 $ BSL.toStrict $ Json.encode packageNamesAndAbsPaths)
        ]
  void $ liftIO $ waitForProcess ph

nixShellWithHoogle :: MonadObelisk m => FilePath -> Bool -> String -> Maybe String -> m ()
nixShellWithHoogle root isPure shell' command = do
  defShellConfig <- nixShellRunConfig root isPure command
  (_, _, _, ph) <- createProcess_ "nixShellWithHoogle" $ setCwd (Just root) $ nixShellRunProc $ defShellConfig
    & nixShellConfig_common . nixCmdConfig_target . target_expr ?~
        "{shell}: ((import ./. {}).passthru.__unstable__.self.extend (_: super: {\
          \userSettings = super.userSettings // { withHoogle = true; };\
        \})).project.shells.${shell}"
    & nixShellConfig_common . nixCmdConfig_args .~ [ strArg "shell" shell' ]
  void $ liftIO $ waitForProcess ph

projectShell :: MonadObelisk m => FilePath -> Bool -> String -> Maybe String -> m ()
projectShell root isPure shellName command = do
  defShellConfig <- nixShellRunConfig root isPure command
  (_, _, _, ph) <- createProcess_ "runNixShellAttr" $ setCwd (Just root) $ nixShellRunProc $ defShellConfig
    & nixShellConfig_common . nixCmdConfig_target . target_path ?~ "default.nix"
    & nixShellConfig_common . nixCmdConfig_target . target_attr ?~ ("shells." <> shellName)
  void $ liftIO $ waitForProcess ph

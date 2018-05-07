{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Project
  ( InitSource (..)
  , initProject
  , findProjectObeliskCommand
  , findProjectRoot
  , withProjectRoot
  , inProjectShell
  , inImpureProjectShell
  , projectShell
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Bits
import Data.Function (on)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.IO
import System.Posix (FileStatus, UserID, deviceID, fileID, fileMode, fileOwner, getFileStatus, getRealUserID)
import System.Posix.Files
import System.Process (CreateProcess, callProcess, createProcess_, cwd, proc, waitForProcess)

import GitHub.Data.GitData (Branch)
import GitHub.Data.Name (Name)

import Obelisk.Command.CLI (withSpinner)
import Obelisk.Command.Thunk

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
  , _gitHubSource_private = True
  }

data InitSource
   = InitSource_Default
   | InitSource_Branch (Name Branch)
   | InitSource_Symlink FilePath

-- | Create a new project rooted in the current directory
initProject :: InitSource -> IO ()
initProject source = do
  let obDir = ".obelisk"
      implDir = obDir </> "impl"
  createDirectory obDir
  case source of
    InitSource_Default -> createThunkWithLatest implDir obeliskSource
    InitSource_Branch branch -> createThunkWithLatest implDir $ obeliskSourceWithBranch branch
    InitSource_Symlink path -> do
      let symlinkPath = if isAbsolute path
            then path
            else ".." </> path
      createSymbolicLink symlinkPath implDir
  _ <- nixBuildAttrWithCache implDir "command"
  --TODO: We should probably handoff to the impl here
  skeleton <- nixBuildAttrWithCache implDir "skeleton" --TODO: I don't think there's actually any reason to cache this
  withSpinner "Copying project skeleton ..." (Just "Copied project skeleton.") $ do
    callProcess "cp" --TODO: Make this package depend on nix-prefetch-url properly
      [ "-r"
      , "--no-preserve=mode"
      , "-T"
      , skeleton </> "."
      , "."
      ]
  let configDir = "config"
  createDirectory configDir
  mapM_ (createDirectory . (configDir </>)) ["backend", "common", "frontend"]
  initGit

initGit :: IO ()
initGit = do
  callProcess "git" ["init"]
  callProcess "git" ["add", "."]
  callProcess "git" ["commit", "-m", "Initial commit."]

--TODO: Handle errors
--TODO: Allow the user to ignore our security concerns
-- | Find the Obelisk implementation for the project at the given path
findProjectObeliskCommand :: FilePath -> IO (Maybe FilePath)
findProjectObeliskCommand target = do
  myUid <- getRealUserID
  targetStat <- liftIO $ getFileStatus target
  (result, insecurePaths) <- flip runStateT [] $ do
    walkToProjectRoot target targetStat myUid >>= \case
      Nothing -> return Nothing
      Just projectRoot -> do
        walkToImplDir projectRoot myUid -- For security check
        return $ Just projectRoot
  case (result, insecurePaths) of
    (Just projDir, []) -> do
      obeliskCommandPkg <- nixBuildAttrWithCache (projDir </> ".obelisk" </> "impl") "command"
      return $ Just $ obeliskCommandPkg </> "bin" </> "ob"
    (Nothing, _) -> return Nothing
    (Just projDir, _) -> do
      T.hPutStr stderr $ T.unlines
        [ "Error: Found a project at " <> T.pack (normalise projDir) <> ", but had to traverse one or more insecure directories to get there:"
        , T.unlines $ fmap (T.pack . normalise) insecurePaths
        , "Please ensure that all of these directories are owned by you and are not writable by anyone else."
        ]
      return Nothing

-- | Get the FilePath to the containing project directory, if there is one
findProjectRoot :: FilePath -> IO (Maybe FilePath)
findProjectRoot target = do
  myUid <- getRealUserID
  targetStat <- liftIO $ getFileStatus target
  (result, _) <- runStateT (walkToProjectRoot target targetStat myUid) []
  return result

withProjectRoot :: FilePath -> (FilePath -> IO ()) -> IO ()
withProjectRoot target f = findProjectRoot target >>= \case
  Nothing -> fail "Must be used inside of an Obelisk project"
  Just root -> f root

-- | Walk from the current directory to the containing project's root directory,
-- if there is one, accumulating potentially insecure directories that were
-- traversed in the process.  Return the project root directory, if found.
walkToProjectRoot :: FilePath -> FileStatus -> UserID -> StateT [FilePath] IO (Maybe FilePath)
walkToProjectRoot this thisStat myUid = liftIO (doesDirectoryExist this) >>= \case
  -- It's not a directory, so it can't be a project
  False -> do
    let dir = takeDirectory this
    dirStat <- liftIO $ getFileStatus dir
    walkToProjectRoot dir dirStat myUid
  True -> do
    when (not $ isWritableOnlyBy thisStat myUid) $ modify (this:)
    liftIO (doesDirectoryExist (this </> ".obelisk")) >>= \case
      True -> return $ Just this
      False -> do
        let next = this </> ".." -- Use ".." instead of chopping off path segments, so that if the current directory is moved during the traversal, the traversal stays consistent
        nextStat <- liftIO $ getFileStatus next
        let fileIdentity fs = (deviceID fs, fileID fs)
            isSameFileAs = (==) `on` fileIdentity
        if thisStat `isSameFileAs` nextStat
          then return Nothing -- Found a cycle; probably hit root directory
          else walkToProjectRoot next nextStat myUid

-- | Walk from the given project root directory to its Obelisk implementation
-- directory, accumulating potentially insecure directories that were traversed
-- in the process.
walkToImplDir :: FilePath -> UserID -> StateT [FilePath] IO ()
walkToImplDir projectRoot myUid = do
  let obDir = projectRoot </> ".obelisk"
  obDirStat <- liftIO $ getFileStatus obDir
  when (not $ isWritableOnlyBy obDirStat myUid) $ modify (obDir:)
  let implThunk = obDir </> "impl"
  implThunkStat <- liftIO $ getFileStatus implThunk
  when (not $ isWritableOnlyBy implThunkStat myUid) $ modify (implThunk:)

--TODO: Is there a better way to ask if anyone else can write things?
--E.g. what about ACLs?
-- | Check to see if directory is only writable by a user whose User ID matches the second argument provided
isWritableOnlyBy :: FileStatus -> UserID -> Bool
isWritableOnlyBy s uid = fileOwner s == uid && fileMode s .&. 0o22 == 0

-- | Run a command in the given shell for the current project
inProjectShell :: String -> String -> IO ()
inProjectShell shellName command = withProjectRoot "." $ \root ->
  projectShell root True shellName command

inImpureProjectShell :: String -> String -> IO ()
inImpureProjectShell shellName command = withProjectRoot "." $ \root ->
  projectShell root False shellName command

projectShell :: FilePath -> Bool -> String -> String -> IO ()
projectShell root isPure shellName command = do
  (_, _, _, ph) <- createProcess_ "runNixShellAttr" $ setCwd (Just root) $ proc "nix-shell" $
     [ "--pure" | isPure ] <>
     [ "-A"
     , "shells." <> shellName
     , "--run", command
     ]
  void $ withSpinner "Project shell ..." (Just "Finishing running project shell (runNixShellAttr)") $ do
    waitForProcess ph

setCwd :: Maybe FilePath -> CreateProcess -> CreateProcess
setCwd fp cp = cp { cwd = fp }

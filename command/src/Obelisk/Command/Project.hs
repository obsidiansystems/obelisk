{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Project
  ( initProject
  , findProjectObeliskCommand
  , findProjectRoot
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
import System.Posix (FileStatus, getFileStatus , deviceID, fileID, getRealUserID , fileOwner, fileMode, UserID)

import GitHub.Data.Name (Name)
import GitHub.Data.GitData (Branch)

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

initProject :: FilePath -> Maybe (Name Branch) -> IO ()
initProject target branch = do
  let obDir = target </> ".obelisk"
      implDir = obDir </> "impl"
  createDirectory obDir
  createThunkWithLatest implDir $ maybe obeliskSource obeliskSourceWithBranch branch
  _ <- nixBuildThunkAttrWithCache implDir "command"
  return ()

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
            -- accumulate insecure directories visited along the way
            -- insecure directories provide an opportunity for attackers
            -- to trick someone into running unexpected code
            let obDir = projectRoot </> ".obelisk"
            obDirStat <- liftIO $ getFileStatus obDir
            when (not $ isSecure obDirStat myUid) $ modify (obDir:)
            let implThunk = obDir </> "impl"
            implThunkStat <- liftIO $ getFileStatus implThunk
            when (not $ isSecure implThunkStat myUid) $ modify (implThunk:)
            return $ Just projectRoot
  case (result, insecurePaths) of
    (Just projDir, []) -> do
       obeliskCommandPkg <- nixBuildThunkAttrWithCache (projDir </> ".obelisk" </> "impl") "command"
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

-- | Get the FilePath to the containing project directory, if there is one
walkToProjectRoot :: FilePath -> FileStatus -> UserID -> StateT [FilePath] IO (Maybe FilePath)
walkToProjectRoot this thisStat myUid = liftIO (doesDirectoryExist this) >>= \case
  -- It's not a directory, so it can't be a project
  False -> do
    let dir = takeDirectory this
    dirStat <- liftIO $ getFileStatus dir
    walkToProjectRoot dir dirStat myUid
  True -> do
    when (not $ isSecure thisStat myUid) $ modify (this:)
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

--TODO: Is there a better way to ask if anyone else can write things?
--E.g. what about ACLs?
isSecure :: FileStatus -> UserID -> Bool
isSecure s uid = fileOwner s == uid && fileMode s .&. 0o22 == 0

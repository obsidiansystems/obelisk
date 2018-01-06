{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Project
  ( initProject
  , findProjectObeliskCommand
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
import System.Exit
import System.FilePath
import System.IO
import System.Posix (FileStatus, getFileStatus , deviceID, fileID, getRealUserID , fileOwner, fileMode)
import System.Process

import Obelisk.Command.Thunk

--TODO: Make this module resilient to random exceptions

--TODO: Don't hardcode this
-- | Source for the Obelisk project
obeliskSource :: ThunkSource
obeliskSource = ThunkSource_GitHub $ GitHubSource
  { _gitHubSource_owner = "obsidiansystems"
  , _gitHubSource_repo = "obelisk"
  , _gitHubSource_branch = Just "master"
  , _gitHubSource_private = True
  }

initProject :: FilePath -> IO ()
initProject target = do
  let obDir = target </> ".obelisk"
      implDir = obDir </> "impl"
  createDirectory obDir
  createThunkWithLatest implDir obeliskSource
  _ <- nixBuildThunkAttrWithCache implDir "command"
  --TODO: We should probably handoff to the impl here
  skeleton <- nixBuildThunkAttrWithCache implDir "skeleton" --TODO: I don't think there's actually any reason to cache this
  (_, _, _, p) <- runInteractiveProcess "cp" --TODO: Make this package depend on nix-prefetch-url properly
    [ "-r"
    , "--no-preserve=mode"
    , "-T"
    , skeleton </> "."
    , target
    ] Nothing Nothing
  ExitSuccess <- waitForProcess p
  return ()

--TODO: Handle errors
--TODO: Allow the user to ignore our security concerns
-- | Find the Obelisk implementation for the project at the given path
findProjectObeliskCommand :: FilePath -> IO (Maybe FilePath)
findProjectObeliskCommand target = do
  myUid <- getRealUserID
  let --TODO: Is there a better way to ask if anyone else can write things?
      --E.g. what about ACLs?
      isSecure s = fileOwner s == myUid && fileMode s .&. 0o22 == 0
      -- | Get the FilePath to the containing project directory, if there is
      -- one; accumulate insecure directories we visited along the way
      findImpl :: FilePath -> FileStatus -> StateT [FilePath] IO (Maybe FilePath)
      findImpl this thisStat = liftIO (doesDirectoryExist this) >>= \case
        -- It's not a directory, so it can't be a project
        False -> do
          let dir = takeDirectory this
          dirStat <- liftIO $ getFileStatus dir
          findImpl dir dirStat
        True -> do
          when (not $ isSecure thisStat) $ modify (this:)
          liftIO (doesDirectoryExist (this </> ".obelisk")) >>= \case
            True -> do
              let obDir = this </> ".obelisk"
              obDirStat <- liftIO $ getFileStatus obDir
              when (not $ isSecure obDirStat) $ modify (obDir:)
              let implThunk = obDir </> "impl"
              implThunkStat <- liftIO $ getFileStatus implThunk
              when (not $ isSecure implThunkStat) $ modify (implThunk:)
              return $ Just this
            False -> do
              let next = this </> ".." -- Use ".." instead of chopping off path segments, so that if the current directory is moved during the traversal, the traversal stays consistent
              nextStat <- liftIO $ getFileStatus next
              let fileIdentity fs = (deviceID fs, fileID fs)
                  isSameFileAs = (==) `on` fileIdentity
              if thisStat `isSameFileAs` nextStat
                then return Nothing -- Found a cycle; probably hit root directory
                else findImpl next nextStat
  targetStat <- liftIO $ getFileStatus target
  (result, insecurePaths) <- runStateT (findImpl target targetStat) []
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

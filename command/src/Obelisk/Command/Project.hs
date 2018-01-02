{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Project
  ( initProject
  , findProjectObeliskCommand
  ) where

import Control.Monad
import Data.Bits
import qualified Data.ByteString.Lazy as LBS
import Data.Function (on)
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Obelisk.Command.Thunk
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Posix (FileStatus, getFileStatus, deviceID, fileID, getRealUserID, fileOwner, fileMode)
import System.Process
import Control.Monad.IO.Class
import Control.Monad.Trans.State

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
  createDirectory obDir
  createThunkWithLatest (obDir </> "impl") obeliskSource

nixBuildDashA :: FilePath -> String -> IO FilePath
nixBuildDashA path attr = do
  -- We need to keep 'err' around, because nix-build seems to crash if it runs
  -- for long enough and we've let 'err' die.  It's probably dying from the
  -- closed output pipe, but I haven't investigated.  The code below that
  -- outputs the contents of 'err' when an error is hit is sufficient to keep it
  -- alive.
  (_, out, err, p) <- runInteractiveProcess "nix-build" --TODO: Make this package depend on nix-prefetch-url properly
    [ "--no-out-link"
    , path
    , "-A"
    , attr
    ] Nothing Nothing
  waitForProcess p >>= \case
    ExitSuccess -> return ()
    _ -> do
      LBS.putStr =<< LBS.hGetContents err
      fail "nix-build failed"
  T.unpack . T.strip <$> T.hGetContents out

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
       obeliskCommandPkg <- nixBuildDashA (projDir </> ".obelisk" </> "impl") "command"
       return $ Just $ obeliskCommandPkg </> "bin" </> "ob"
    (Nothing, _) -> return Nothing
    (Just projDir, _) -> do
      T.hPutStr stderr $ T.unlines
        [ "Error: Found a project at " <> T.pack (normalise projDir) <> ", but had to traverse one or more insecure directories to get there:"
        , T.unlines $ fmap (T.pack . normalise) insecurePaths
        , "Please ensure that all of these directories are owned by you and are not writable by anyone else."
        ]
      return Nothing

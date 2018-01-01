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
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Obelisk.Command.Thunk
import System.Directory
import System.Exit
import System.FilePath
import System.Posix (getFileStatus, deviceID, fileID, getRealUserID, fileOwner, fileMode)
import System.Process

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
    [ path
    , "-A"
    , attr
    ] Nothing Nothing
  waitForProcess p >>= \case
    ExitSuccess -> return ()
    _ -> LBS.putStr =<< LBS.hGetContents err
  T.unpack . T.strip <$> T.hGetContents out

--TODO: Handle errors
--TODO: Allow the user to ignore our security concerns
-- | Find the Obelisk implementation for the project at the given path
findProjectObeliskCommand :: FilePath -> IO (Maybe FilePath)
findProjectObeliskCommand target = do
  myUid <- getRealUserID
  let isSecure s = fileOwner s == myUid && fileMode s .&. 0o22 == 0
      go this = doesDirectoryExist this >>= \case
        -- It's not a directory, so it can't be a project
        False -> go $ takeDirectory this
        True -> do
          thisStat <- getFileStatus this
          guard $ isSecure thisStat
          doesDirectoryExist (this </> ".obelisk") >>= \case
            True -> do
              let obDir = this </> ".obelisk"
              obDirStat <- getFileStatus obDir
              guard $ isSecure obDirStat
              let implThunk = obDir </> "impl"
              implThunkStat <- getFileStatus implThunk
              guard $ isSecure implThunkStat
              obeliskCommandPkg <- nixBuildDashA implThunk "command"
              return $ Just $ obeliskCommandPkg </> "bin" </> "ob"
            False -> do
              let next = this </> ".." -- Use ".." instead of chopping off path segments, so that if the current directory is moved during the traversal, the traversal stays consistent
              nextStat <- getFileStatus next
              let fileIdentity fs = (deviceID fs, fileID fs)
                  isSameFileAs = (==) `on` fileIdentity
              if thisStat `isSameFileAs` nextStat
                then return Nothing -- Found a cycle; probably hit root directory
                else go next
  go target

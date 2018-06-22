{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Command.Upgrade.Hash
  ( getDirectoryHash
  , getHashAtGitRevision
  ) where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.IO.Temp
import System.Process (cwd, proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Utils

import Obelisk.Migration

-- | Get the unique hash of the given directory
--
-- Excludes the following before computing the hash:
-- * the specified top-level files/ directories.
-- * .git directory
-- * untracked Git files
-- * ignored Git files
--
-- Uses the same predictive algorithm that Nix (`nix hash-path`).
--
-- This function will do a full copy of the directory to a temporary location before
-- computing the hash. Because it will be deleting the files in exclude list, and
-- other files if the directory is a git repo. This needs to be done as `nix hash-path`
-- doesn't support taking an excludes list.
getDirectoryHash :: MonadObelisk m => [FilePath] -> FilePath -> m Hash
getDirectoryHash excludes dir = withSystemTempDirectory "obelisk-hash-" $ \tmpDir -> do
  withSpinnerNoTrail (T.pack $ "Copying " <> dir <> " to " <> tmpDir) $ do
    runProc $ copyDir dir tmpDir
  getDirectoryHashDestructive excludes tmpDir

-- Do /not/ call this directly! Call `getDirectoryHash` instead.
getDirectoryHashDestructive :: MonadObelisk m => [FilePath] -> FilePath -> m Hash
getDirectoryHashDestructive excludes dir = do
  liftIO (doesDirectoryExist $ dir </> ".git") >>= \case
    True -> do
      tidyUpGitWorkingCopy dir
      withSpinnerNoTrail "Removing .git directory" $
        liftIO $ removePathForcibly $ dir </> ".git"
    False -> pure ()
  withSpinnerNoTrail "Removing excluded paths" $ do
    forM_ (fmap (dir </>) excludes) $
      liftIO . removePathForcibly
  nixHash dir

getHashAtGitRevision :: MonadObelisk m => [Text] -> [FilePath] -> FilePath -> m [Hash]
getHashAtGitRevision revs excludes dir = withSystemTempDirectory "obelisk-hashrev-" $ \tmpDir -> do
  withSpinner (T.pack $ "Copying " <> dir <> " to " <> tmpDir) $ do
    runProc $ copyDir dir tmpDir
  tidyUpGitWorkingCopy tmpDir
  -- Discard changes to tracked files
  runProcSilently $ gitProc tmpDir ["reset", "--hard"]
  forM revs $ \rev -> do
    runProcSilently $ gitProc tmpDir ["checkout", T.unpack rev]
    -- Checking out an arbitrary revision can leave untracked files (from
    -- previous revison) around, so tidy them up.
    tidyUpGitWorkingCopy tmpDir
    withFilesStashed tmpDir (excludes <> [".git"]) $
      nixHash tmpDir
  where
    withFilesStashed base fs m = withSystemTempDirectory "obelisk-hashrev-stash-" $ \stashDir -> do
      existingPaths <- fmap catMaybes $ forM fs $ \p -> do
        liftIO (doesPathExist $ base </> p) >>= pure . bool Nothing (Just p)
      forM_ existingPaths $ \p ->
        liftIO $ renamePath (base </> p) (stashDir </> p)
      result <- m
      forM_ existingPaths $ \p ->
        liftIO $ renamePath (stashDir </> p) (base </> p)
      return result

nixHash :: MonadObelisk m => FilePath -> m Hash
nixHash dir = withSpinnerNoTrail "Running `nix hash-path`" $
  readProc $ proc "nix" ["hash-path", "--type", "md5", dir]

-- | Clean up the following files in the git working copy
--
-- * Paths ignored by .gitignored, but still present in the filesystem
-- * Untracked files (not added to git index)
-- * Any empty directories (these are not tracked by git)
--
-- Note that this leaves modified (staged or unstaged) files as they are.
tidyUpGitWorkingCopy :: MonadObelisk m => FilePath -> m ()
tidyUpGitWorkingCopy dir = withSpinnerNoTrail "Tidying up git working copy" $ do
  ignored <- gitLsFiles dir ["--ignored", "--exclude-standard", "--others"]
  untracked <- gitLsFiles dir ["--exclude-standard", "--others"]
  putLog Debug $ T.pack $ "Found " <> show (length ignored) <> " ignored files."
  putLog Debug $ T.pack $ "Untracked:\n" <> unlines untracked
  putLog Debug $ T.pack $ "Ignored:\n" <> unlines ignored
  withSpinnerNoTrail "Removing untracked and ignored files" $ do
    forM_ (fmap (dir </>) $ ignored <> untracked) $
      liftIO . removePathForcibly
  -- Empty directories won't be included in these lists. Git doesn't track them
  -- So we must delete these separately.
  runProc $ proc "find" [dir, "-depth", "-empty", "-type", "d", "-delete"]
  where
    gitLsFiles pwd opts = fmap lines $ readProcessAndLogStderr Error $
      (proc "git" $ ["ls-files", "."] <> opts) { cwd = Just pwd }


{-# LANGUAGE ScopedTypeVariables #-}
-- | Shared utilities for Obelisk Setup.hs hooks.
module Obelisk.Setup.Utils
  ( findProjectRoot
  , symlink
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import System.Directory
  ( createFileLink
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , pathIsSymbolicLink
  , removeFile
  )
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)

-- | Walk up from the current directory until a @cabal.project@ file is found.
findProjectRoot :: IO FilePath
findProjectRoot = getCurrentDirectory >>= go
  where
    go dir = do
      exists <- doesFileExist (dir </> "cabal.project")
      if exists
        then pure dir
        else let parent = takeDirectory dir
             in if parent == dir
                then fail "[Setup] Could not find cabal.project (project root)"
                else go parent

-- | Create a symlink idempotently. Removes stale symlinks; skips if a real
-- directory already exists at the target location.
symlink :: FilePath -> FilePath -> IO ()
symlink target linkName = do
  isLink <- pathIsSymbolicLink linkName `catch` \(_ :: IOException) -> pure False
  when isLink $ removeFile linkName
  dirExists <- doesDirectoryExist linkName
  unless dirExists $ do
    createFileLink target linkName
    hPutStrLn stderr $ "[Setup] Symlinked " <> linkName <> " -> " <> target

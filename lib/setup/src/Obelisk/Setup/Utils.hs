{-# LANGUAGE ScopedTypeVariables #-}
-- | Shared utilities for Obelisk Setup.hs hooks.
module Obelisk.Setup.Utils
  ( findProjectRoot
  , symlink
  , crossCabalArgs
  , optLevelFlags
  , strip
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (unless, when)
import Distribution.Simple.Compiler (OptimisationLevel (..))
import System.Environment (lookupEnv)

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

-- | Read extra cabal arguments for cross builds from @OBELISK_CROSS_CABAL_ARGS@.
crossCabalArgs :: IO [String]
crossCabalArgs = maybe [] words <$> lookupEnv "OBELISK_CROSS_CABAL_ARGS"

-- | Map a Cabal 'OptimisationLevel' to @--ghc-options@ flags for cross cabal.
optLevelFlags :: OptimisationLevel -> [String]
optLevelFlags NoOptimisation      = ["--ghc-options=-O0"]
optLevelFlags NormalOptimisation   = ["--ghc-options=-O1"]
optLevelFlags MaximumOptimisation  = ["--ghc-options=-O2"]

-- | Strip leading and trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

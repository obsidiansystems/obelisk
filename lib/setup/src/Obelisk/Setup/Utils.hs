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

-- | Map a Cabal 'OptimisationLevel' to cabal-level optimization flags.
-- Uses @-O@ rather than @--ghc-options=-O@ so that cabal's build directory
-- layout (e.g. @noopt/@) matches the optimization level.
optLevelFlags :: OptimisationLevel -> [String]
optLevelFlags NoOptimisation      = ["-O0"]
optLevelFlags NormalOptimisation   = ["-O1"]
optLevelFlags MaximumOptimisation  = ["-O2"]

-- | Strip leading and trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

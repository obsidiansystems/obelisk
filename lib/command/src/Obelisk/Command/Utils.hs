{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import System.Process (CreateProcess, callProcess, proc, readProcess)

-- Check whether the working directory is clean
checkGitCleanStatus :: FilePath -> IO Bool
checkGitCleanStatus fp = do
  statusOutput <- readProcess "hub"
    [ "-C", fp
    , "status", "--porcelain", "--ignored" ] ""
  diffOutput <- readProcess "hub" [ "-C", fp , "diff" ] ""
  return $ null statusOutput && null diffOutput

initGit :: FilePath -> IO ()
initGit dir = do
  let git args = callProcess "git" $ "-C" : dir : args
  git ["init"]
  git ["add", "."]
  git ["commit", "-m", "Initial commit."]

-- | Like `System.Process.proc` but with the specified Nix packages installed
procWithPackages :: [String] -> FilePath -> [String] -> CreateProcess
procWithPackages pkgs cmd args = proc "nix-shell" $
  "-p" : pkgs <> ["--run", unwords $ cmd : map quoteAndEscape args]
  where
    quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

-- | Portable `cp` command
cp :: [String] -> CreateProcess
cp = procWithPackages ["coreutils"] "cp"

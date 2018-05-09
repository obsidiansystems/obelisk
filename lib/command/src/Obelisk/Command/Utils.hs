module Obelisk.Command.Utils where

import System.Process

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

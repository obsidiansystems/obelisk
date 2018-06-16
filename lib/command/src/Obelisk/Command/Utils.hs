{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import Control.Applicative (liftA2)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified System.Process as P

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..), callProcessAndLogOutput, readProcessAndLogStderr)

-- Check whether the working directory is clean
checkGitCleanStatus :: MonadObelisk m => FilePath -> m Bool
checkGitCleanStatus repo = do
  let git = readProcessAndLogStderr Debug . gitProc repo
  null <$> liftA2 (<>) (git ["status", "--porcelain", "--ignored"]) (git ["diff"])

initGit :: MonadObelisk m => FilePath -> m ()
initGit repo = do
  let git = callProcessAndLogOutput (Debug, Debug) . gitProc repo
  git ["init"]
  git ["add", "."]
  git ["commit", "-m", "Initial commit."]

gitProc :: FilePath -> [String] -> P.CreateProcess
gitProc repo argsRaw =
  P.proc "git" $ runGitInDir argsRaw
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

processToShellString :: FilePath -> [String] -> String
processToShellString cmd args = unwords $ map quoteAndEscape (cmd : args)
  where quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

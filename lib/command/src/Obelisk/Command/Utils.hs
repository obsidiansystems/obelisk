{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import Control.Applicative (liftA2)
import Data.Bool (bool)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Process as P

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

-- Check whether the working directory is clean
checkGitCleanStatus :: MonadObelisk m => FilePath -> Bool -> m Bool
checkGitCleanStatus repo withIgnored = do
  let
    runGit = readProcessAndLogStderr Debug . gitProc repo
    gitStatus = runGit $ ["status", "--porcelain"] <> bool [] ["--ignored"] withIgnored
    gitDiff = runGit ["diff"]
  null <$> liftA2 (<>) gitStatus gitDiff

-- | Ensure that git repo is clean
ensureCleanGitRepo :: MonadObelisk m => FilePath -> Bool -> Text -> m ()
ensureCleanGitRepo path withIgnored s =
  withSpinnerNoTrail ("Ensuring clean git repo at " <> T.pack path) $ do
    checkGitCleanStatus path withIgnored >>= \case
      False -> do
        statusDebug <- readGitProcess path $ ["status"] <> bool [] ["--ignored"] withIgnored
        putLog Warning "Working copy is unsaved; git status:"
        putLog Notice statusDebug
        failWith s
      True -> pure ()

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

-- | Recursively copy a directory using `cp -a`
copyDir :: FilePath -> FilePath -> P.CreateProcess
copyDir src dest =
  (P.proc "cp" ["-a", ".", dest]) { P.cwd = Just src }

readGitProcess :: MonadObelisk m => FilePath -> [String] -> m Text
readGitProcess repo = fmap T.pack . readProcessAndLogStderr Notice . gitProc repo

processToShellString :: FilePath -> [String] -> String
processToShellString cmd args = unwords $ map quoteAndEscape (cmd : args)
  where quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

-- | A simpler wrapper for CliApp's most used process function with sensible defaults.
runProc :: MonadObelisk m => P.CreateProcess -> m ()
runProc = callProcessAndLogOutput (Notice, Error)

-- | Like runProc, but all output goes to Debug logging level
runProcSilently :: MonadObelisk m => P.CreateProcess -> m ()
runProcSilently = callProcessAndLogOutput (Debug, Debug)

-- | A simpler wrapper for CliApp's readProcessAndLogStderr with sensible defaults.
readProc :: MonadObelisk m => P.CreateProcess -> m Text
readProc = fmap T.pack . readProcessAndLogStderr Error

tshow :: Show a => a -> Text
tshow = T.pack . show

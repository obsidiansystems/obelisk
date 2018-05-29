{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified System.Process as P

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..), callProcessAndLogOutput, readProcessAndLogStderr)

-- Check whether the working directory is clean
checkGitCleanStatus :: MonadObelisk m => FilePath -> m Bool
checkGitCleanStatus repo = do
  out <- readProcessAndLogStderr Debug $ git repo [["status", "--porcelain", "--ignored"], ["diff"]]
  pure $ null out

initGit :: MonadObelisk m => FilePath -> m ()
initGit dir = callProcessAndLogOutput (Debug, Debug) $ git dir
  [ ["init"]
  , ["add", "."]
  , ["commit", "-m", "Initial commit."]
  ]

git :: FilePath -> [[String]] -> P.CreateProcess
git repo argss =
  inNixShell ["git"] $ L.intercalate "; " $
    map (processToShellString "git" . runGitInDir) $ filter (not . null) argss
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

-- | Like `System.Process.proc` but with the specified Nix packages installed
procWithPackages :: [String] -> FilePath -> [String] -> P.CreateProcess
procWithPackages pkgs cmd args = inNixShell pkgs $ processToShellString cmd args

inNixShell :: [String] -> String -> P.CreateProcess
inNixShell pkgs cmd = P.proc "nix-shell" $ "-p" : pkgs <> ["--run", cmd]

processToShellString :: FilePath -> [String] -> String
processToShellString cmd args = unwords $ map quoteAndEscape (cmd : args)
  where quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

-- | Portable `cp` command.
cp :: [String] -> P.CreateProcess
cp = procWithPackages ["coreutils"] "cp"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import Data.Bool (bool)
import qualified Data.List as L
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Process as P

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

-- Check whether the working directory is clean
checkGitCleanStatus :: MonadObelisk m => FilePath -> Bool -> m Bool
checkGitCleanStatus repo withIgnored = do
  out <- readProcessAndLogStderr Debug $
    git repo
    [ ["status", "--porcelain"] <> bool [] ["--ignored"] withIgnored
    , ["diff"]
    ]
  pure $ null out

-- | Ensure that git repo is clean
ensureCleanGitRepo :: MonadObelisk m => FilePath -> Bool -> Text -> m ()
ensureCleanGitRepo path withIgnored s =
  withSpinnerNoTrail ("Ensuring clean git repo at " <> T.pack path) $ do
    checkGitCleanStatus path withIgnored >>= \case
      False -> do
        statusDebug <- fmap T.pack $ readProcessAndLogStderr Notice $
          git1 path $ ["status"] <> bool [] ["--ignored"] withIgnored
        putLog Warning "Working copy is unsaved; git status:"
        putLog Notice statusDebug
        failWith s
      True -> pure ()

initGit :: MonadObelisk m => FilePath -> m ()
initGit dir = callProcessAndLogOutput (Debug, Debug) $ git dir
  [ ["init"]
  , ["add", "."]
  , ["commit", "-m", "Initial commit."]
  ]

-- | Run several `git` commands under the given repo.
git :: FilePath -> [[String]] -> P.CreateProcess
git repo argss =
  inNixShell ["git"] $ L.intercalate "; " $
    map (processToShellString "git" . runGitInDir) $ filter (not . null) argss
  where
    runGitInDir args' = case filter (not . null) args' of
      args@("clone":_) -> args <> [repo]
      args -> ["-C", repo] <> args

-- | Like `git` but just for one command.
git1 :: FilePath -> [String] -> P.CreateProcess
git1 repo args = git repo [args]

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

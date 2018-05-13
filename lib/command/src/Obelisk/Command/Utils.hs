{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Utils where

import qualified Data.List as L
import Data.Semigroup ((<>))
import qualified Data.Text as T
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

callProcessNixShell :: [String] -> FilePath -> [String] -> IO ()
callProcessNixShell pkgs cmd args = callProcess "nix-shell" $
  "-p" : pkgs <> ["--run", L.intercalate " " $ cmd : map quoteAndEscape args]
  where
    quoteAndEscape x = T.unpack $ "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

cp :: [String] -> IO ()
cp = callProcessNixShell ["coreutils"] "cp"

{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Repl
  ( runRepl
  , watch
  ) where

import Data.Monoid ((<>))

import Obelisk.Command.Project

-- | Run an interactive repl within a given path
runRepl :: FilePath -> IO ()
runRepl dir = inProjectShell "ghc" $ "cd " <> dir <> "; cabal new-repl " <> dir

-- | Watch the given directory for errors and warnings
watch :: FilePath -> IO ()
watch dir = inProjectShell "ghc" $ "cd " <> dir <> "; ghcid -W -c'cabal new-repl " <> dir <> "'"

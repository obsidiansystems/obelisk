{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Repl
  ( runRepl
  , watch
  ) where

import Data.Monoid ((<>))

import Obelisk.App (MonadObelisk)
import Obelisk.Command.Project

-- | Run an interactive repl within a given path
runRepl :: MonadObelisk m => FilePath -> m ()
runRepl dir = inProjectShell "ghc" $ "cd " <> dir <> "; cabal new-repl"

-- | Watch the given directory for errors and warnings
watch :: MonadObelisk m => FilePath -> m ()
watch dir = inProjectShell "ghc" $ "cd " <> dir <> "; ghcid -W --command='cabal new-repl'"


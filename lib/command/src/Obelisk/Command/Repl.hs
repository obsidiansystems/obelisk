{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Repl
  ( watch
  ) where

import Data.Monoid ((<>))

import Obelisk.App (MonadObelisk)
import Obelisk.Command.Project

-- | Watch the given directory for errors and warnings
watch :: MonadObelisk m => FilePath -> m ()
watch dir = inProjectShell "ghc" $ "cd " <> dir <> "; ghcid -W --command='cabal new-repl'"

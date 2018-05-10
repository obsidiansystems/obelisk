{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StaticPointers #-}
module Obelisk.App where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (MonadIO, MonadReader)

-- TODO: Add configuration for logging, etc. here.
data Obelisk = Obelisk
  { _obelisk_verbose :: Bool
  }
  deriving (Eq, Show)

type MonadObelisk m = (MonadReader Obelisk m, MonadIO m, MonadMask m)

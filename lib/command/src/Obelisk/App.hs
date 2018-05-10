{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StaticPointers #-}
module Obelisk.App where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (MonadIO, MonadReader)

-- TODO: Add configuration for logging, etc. here.
data Obelisk = Obelisk
  { _obelisk_version :: String
  }

type MonadObelisk m = (MonadReader Obelisk m, MonadIO m, MonadMask m)

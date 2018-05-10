{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.App where

import Control.Monad.Catch (MonadMask)
import Control.Monad.Reader (MonadIO, MonadReader)

data Obelisk = Obelisk
  { _obelisk_verbose :: Bool
  }
  deriving (Eq, Show)

type MonadObelisk m = (MonadReader Obelisk m, MonadIO m, MonadMask m)

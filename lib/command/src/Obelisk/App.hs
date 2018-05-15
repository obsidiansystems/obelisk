{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT, MonadLog, runLoggingT)
import Control.Monad.Reader (MonadIO, MonadReader, ReaderT, runReaderT)

import Obelisk.CLI.Logging (LoggingConfig, Output, handleLog)

data Obelisk = Obelisk
  { _obelisk_noSpinner :: Bool  -- Used to disable the spinner.
  , _obelisk_logging :: LoggingConfig
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (LoggingT Output m) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadLog Output)

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    flip runLoggingT loggingConfig
  . flip runReaderT c
  . unObeliskT
  where
    loggingConfig = handleLog $ _obelisk_logging c

type MonadObelisk m = (MonadReader Obelisk m, MonadIO m, MonadMask m, MonadLog Output m)

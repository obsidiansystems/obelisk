{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Obelisk.App where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader (MonadIO, ReaderT (..), ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import UnliftIO (MonadUnliftIO, UnliftIO (..), askUnliftIO, withUnliftIO)

import Obelisk.CLI (Cli, CliConfig, CliT, HasCliConfig, getCliConfig, runCli)

newtype Obelisk = Obelisk
  { _obelisk_cliConfig :: CliConfig
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (CliT m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask
    , HasObelisk, HasCliConfig)

deriving instance Monad m => Cli (ObeliskT m)

class HasObelisk m where
  getObelisk :: m Obelisk

instance Monad m => HasObelisk (ReaderT Obelisk m) where
  getObelisk = ask

instance HasCliConfig m => HasCliConfig (ReaderT Obelisk m) where
  getCliConfig = lift getCliConfig

instance MonadUnliftIO m => MonadUnliftIO (ObeliskT m) where
  askUnliftIO = ObeliskT $ withUnliftIO $ \u ->
    return (UnliftIO (unliftIO u . unObeliskT))

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    runCli (_obelisk_cliConfig c)
  . flip runReaderT c
  . unObeliskT

type MonadObelisk m =
  ( Cli m
  , HasCliConfig m
  , HasObelisk m
  , MonadUnliftIO m
  , MonadMask m
  )

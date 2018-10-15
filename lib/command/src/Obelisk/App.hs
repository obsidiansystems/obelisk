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
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import Control.Monad.Log (MonadLog)

import Obelisk.CliApp (CliConfig, CliT, HasCliConfig, runCli, Output)

newtype Obelisk = Obelisk
  { _obelisk_cliConfig :: CliConfig
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (CliT m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask
    , HasObelisk, HasCliConfig)

deriving instance Monad m => MonadLog Output (ObeliskT m)

class HasObelisk m where
  getObelisk :: m Obelisk

instance Monad m => HasObelisk (ReaderT Obelisk m) where
  getObelisk = ask

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    runCli (_obelisk_cliConfig c)
  . flip runReaderT c
  . unObeliskT

type MonadObelisk m =
  ( MonadLog Output m
  , HasCliConfig m
  , HasObelisk m
  , MonadIO m
  , MonadMask m
  )

getObeliskUserStateDir :: IO FilePath
getObeliskUserStateDir = getXdgDirectory XdgData "obelisk"

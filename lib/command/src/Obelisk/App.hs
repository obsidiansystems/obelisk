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
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Text (Text)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import Control.Monad.Log (MonadLog)

import Obelisk.CliApp (CliConfig, CliLog, CliThrow, CliT (..), HasCliConfig, runCli, Output, ProcessFailed)

newtype Obelisk = Obelisk
  { _obelisk_cliConfig :: CliConfig
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (CliT m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask
    , MonadLog Output -- CliLog
    , MonadError (Either Text ProcessFailed) -- CliThrow
    , HasCliConfig)

instance MonadTrans ObeliskT where
  lift = ObeliskT . lift . lift

class Monad m => HasObelisk m where
  getObelisk :: m Obelisk

instance Monad m => HasObelisk (ObeliskT m) where
  getObelisk = ObeliskT ask

instance HasObelisk m => HasObelisk (ReaderT r m) where
  getObelisk = lift getObelisk

instance (Monoid w, HasObelisk m) => HasObelisk (WriterT w m) where
  getObelisk = lift getObelisk

instance HasObelisk m => HasObelisk (StateT r m) where
  getObelisk = lift getObelisk

instance HasObelisk m => HasObelisk (ExceptT e m) where
  getObelisk = lift getObelisk

runObelisk :: MonadIO m => Obelisk -> ObeliskT m a -> m a
runObelisk c =
    runCli (_obelisk_cliConfig c)
  . flip runReaderT c
  . unObeliskT

type MonadInfallibleObelisk m =
  ( CliLog m
  , HasCliConfig m
  , HasObelisk m
  , MonadIO m
  , MonadMask m
  )

type MonadObelisk m =
  ( MonadInfallibleObelisk m
  , CliThrow m
  )

getObeliskUserStateDir :: IO FilePath
getObeliskUserStateDir = getXdgDirectory XdgData "obelisk"

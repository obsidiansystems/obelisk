{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.App where

import Control.Lens
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (MonadIO, ReaderT (..), ask, runReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Text (Text)
import System.Directory (XdgDirectory (XdgData), getXdgDirectory)
import Control.Monad.Log (MonadLog)

import Cli.Extras
  ( CliConfig
  , CliLog
  , CliThrow
  , CliT (..)
  , ProcessFailure
  , AsProcessFailure (..)
  , AsUnstructuredError (..)
  , HasCliConfig
  , Output
  , runCli
  )

data ObeliskError
  = ObeliskError_ProcessError
    { obeliskError_err :: ProcessFailure
    , obeliskError_mAnn :: Maybe Text
    }
  | ObeliskError_Unstructured Text

makePrisms ''ObeliskError

instance AsUnstructuredError ObeliskError where
  asUnstructuredError = _ObeliskError_Unstructured

-- Only project when the other field is null, otherwise we are not law abiding.
instance AsProcessFailure ObeliskError where
  asProcessFailure = _ObeliskError_ProcessError . prism (, Nothing) f
    where f = \case
            (pf, Nothing) -> Right pf
            pann@(_, Just _) -> Left pann

newtype Obelisk = Obelisk
  { _obelisk_cliConfig :: CliConfig ObeliskError
  }

newtype ObeliskT m a = ObeliskT
  { unObeliskT :: ReaderT Obelisk (CliT ObeliskError m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask, MonadFail
    , MonadLog Output -- CliLog
    , MonadError ObeliskError -- CliThrow ObeliskError
    , HasCliConfig ObeliskError
    )

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
  , HasCliConfig ObeliskError m
  , HasObelisk m
  , MonadIO m
  , MonadMask m
  )

type MonadObelisk m =
  ( MonadInfallibleObelisk m
  , CliThrow ObeliskError m
  , MonadFail m
  )

getObeliskUserStateDir :: IO FilePath
getObeliskUserStateDir = getXdgDirectory XdgData "obelisk"

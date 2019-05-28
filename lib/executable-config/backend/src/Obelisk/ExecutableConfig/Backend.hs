{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.ExecutableConfig.Backend
  ( HasBackendConfigs(..)
  , BackendConfigsT(..)
  , runBackendConfigsT
  , mapBackendConfigsT
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Snap.Core (MonadSnap)

class Monad m => HasBackendConfigs m where
  getBackendConfig :: Text -> m (Maybe Text)
  default getBackendConfig :: (HasBackendConfigs m', m ~ t m', MonadTrans t) => Text -> m (Maybe Text)
  getBackendConfig = lift . getBackendConfig
  getCommonConfig :: Text -> m (Maybe Text)
  default getCommonConfig :: (HasBackendConfigs m', m ~ t m', MonadTrans t) => Text -> m (Maybe Text)
  getCommonConfig = lift . getCommonConfig

newtype BackendConfigsT m a = BackendConfigsT { unBackendConfigsT :: ReaderT (Map Text Text) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadPlus
    , Alternative
    , MonadFix
    , MonadIO
    , MonadBase m'
    , MonadBaseControl m'
    , MonadRef
    , MonadTrans
    , MonadSnap
    )

instance PrimMonad m => PrimMonad (BackendConfigsT m) where
  type PrimState (BackendConfigsT m) = PrimState m
  primitive = lift . primitive

runBackendConfigsT
  :: Map Text Text
  -> BackendConfigsT m a
  -> m a
runBackendConfigsT cs child = runReaderT (unBackendConfigsT child) cs

instance Monad m => HasBackendConfigs (BackendConfigsT m) where
  getBackendConfig k = BackendConfigsT $ Map.lookup ("config/backend/" <> k) <$> ask
  getCommonConfig k = BackendConfigsT $ Map.lookup ("config/common/" <> k) <$> ask

mapBackendConfigsT
  :: (forall x. m x -> n x)
  -> BackendConfigsT m a
  -> BackendConfigsT n a
mapBackendConfigsT f (BackendConfigsT x) = BackendConfigsT $ mapReaderT f x

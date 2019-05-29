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
module Obelisk.ExecutableConfig.Frontend
  ( HasFrontendConfigs(..)
  , FrontendConfigsT
  , runFrontendConfigsT
  , mapFrontendConfigsT
  ) where

import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Reflex
import Reflex.Host.Class
import Reflex.Dom.Core
#ifndef ghcjs_HOST_OS
import GHCJS.DOM.Types (MonadJSM)
#endif

class Monad m => HasFrontendConfigs m where
  getFrontendConfig :: Text -> m (Maybe Text)
  default getFrontendConfig :: (HasFrontendConfigs m', m ~ t m', MonadTrans t) => Text -> m (Maybe Text)
  getFrontendConfig = lift . getFrontendConfig
  getCommonConfig :: Text -> m (Maybe Text)
  default getCommonConfig :: (HasFrontendConfigs m', m ~ t m', MonadTrans t) => Text -> m (Maybe Text)
  getCommonConfig = lift . getCommonConfig

instance HasFrontendConfigs m => HasFrontendConfigs (BehaviorWriterT t w m)
instance HasFrontendConfigs m => HasFrontendConfigs (DynamicWriterT t w m)
instance HasFrontendConfigs m => HasFrontendConfigs (EventWriterT t w m)
instance HasFrontendConfigs m => HasFrontendConfigs (PostBuildT t m)
instance HasFrontendConfigs m => HasFrontendConfigs (QueryT t q m)
instance HasFrontendConfigs m => HasFrontendConfigs (ReaderT r m)
instance HasFrontendConfigs m => HasFrontendConfigs (RequesterT t request response m)
instance HasFrontendConfigs m => HasFrontendConfigs (StaticDomBuilderT t m)
instance HasFrontendConfigs m => HasFrontendConfigs (TriggerEventT t m)

newtype FrontendConfigsT m a = FrontendConfigsT { unFrontendConfigsT :: ReaderT (Map Text Text) m a }
  deriving
    ( Functor
    , Applicative
    , DomBuilder t
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
#ifndef ghcjs_HOST_OS
    , MonadJSM
#endif
    , MonadRef
    , MonadReflexCreateTrigger t
    , MonadSample t
    , MonadTrans
    , NotReady t
    , PostBuild t
    , TriggerEvent t
    , HasDocument
    , DomRenderHook t
    , HasJSContext
    , HasJS js
    )

instance PerformEvent t m => PerformEvent t (FrontendConfigsT m) where
  type Performable (FrontendConfigsT m) = FrontendConfigsT (Performable m)
  performEvent e = FrontendConfigsT $ ReaderT $ \configs ->
    performEvent $ runFrontendConfigsT configs <$> e
  performEvent_ e = FrontendConfigsT $ ReaderT $ \configs ->
    performEvent_ $ runFrontendConfigsT configs <$> e

instance Adjustable t m => Adjustable t (FrontendConfigsT m) where
  runWithReplace a e = FrontendConfigsT $ runWithReplace (unFrontendConfigsT a) (unFrontendConfigsT <$> e)
  traverseDMapWithKeyWithAdjust f m e = FrontendConfigsT $ traverseDMapWithKeyWithAdjust (\k v -> unFrontendConfigsT $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = FrontendConfigsT $ traverseIntMapWithKeyWithAdjust (\k v -> unFrontendConfigsT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = FrontendConfigsT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unFrontendConfigsT $ f k v) m e

instance Prerender js t m => Prerender js t (FrontendConfigsT m) where
  type Client (FrontendConfigsT m) = FrontendConfigsT (Client m)
  prerender server client = FrontendConfigsT $ ReaderT $ \configs ->
    prerender (runFrontendConfigsT configs server) (runFrontendConfigsT configs client)

instance PrimMonad m => PrimMonad (FrontendConfigsT m) where
  type PrimState (FrontendConfigsT m) = PrimState m
  primitive = lift . primitive

runFrontendConfigsT
  :: Map Text Text
  -> FrontendConfigsT m a
  -> m a
runFrontendConfigsT cs child = runReaderT (unFrontendConfigsT child) cs

instance Monad m => HasFrontendConfigs (FrontendConfigsT m) where
  getFrontendConfig k = FrontendConfigsT $ Map.lookup ("config/frontend/" <> k) <$> ask
  getCommonConfig k = FrontendConfigsT $ Map.lookup ("config/common/" <> k) <$> ask

mapFrontendConfigsT
  :: (forall x. m x -> n x)
  -> FrontendConfigsT m a
  -> FrontendConfigsT n a
mapFrontendConfigsT f (FrontendConfigsT x) = FrontendConfigsT $ mapReaderT f x

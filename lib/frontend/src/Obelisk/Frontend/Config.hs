{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Obelisk.Frontend.Config where

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
import GHCJS.DOM.Types (MonadJSM)

import Obelisk.Frontend.Cookie (CookiesT, HasCookies)
import Obelisk.Route.Frontend

newtype Configs = Configs { unConfigs :: Map Text Text }

class Monad m => HasConfigs m where
  askConfigs :: m Configs
  default askConfigs :: (HasConfigs m', m ~ t m', MonadTrans t) => m Configs
  askConfigs = lift askConfigs

instance HasConfigs m => HasConfigs (BehaviorWriterT t w m)
instance HasConfigs m => HasConfigs (DynamicWriterT t w m)
instance HasConfigs m => HasConfigs (EventWriterT t w m)
instance HasConfigs m => HasConfigs (PostBuildT t m)
instance HasConfigs m => HasConfigs (QueryT t q m)
instance HasConfigs m => HasConfigs (ReaderT r m)
instance HasConfigs m => HasConfigs (RequesterT t request response m)
instance HasConfigs m => HasConfigs (RouteToUrlT t m)
instance HasConfigs m => HasConfigs (SetRouteT t r m)
instance HasConfigs m => HasConfigs (StaticDomBuilderT t m)
instance HasConfigs m => HasConfigs (TriggerEventT t m)
instance HasConfigs m => HasConfigs (RoutedT t r m)
instance HasConfigs m => HasConfigs (CookiesT m)
instance HasCookies m => HasCookies (ConfigsT m)

newtype ConfigsT m a = ConfigsT { unConfigsT :: ReaderT Configs m a }
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
    , PerformEvent t
    , PostBuild t
    , Prerender js t
    , TriggerEvent t
    , HasDocument
    )

instance Adjustable t m => Adjustable t (ConfigsT m) where
  runWithReplace a e = ConfigsT $ runWithReplace (unConfigsT a) (unConfigsT <$> e)
  traverseDMapWithKeyWithAdjust f m e = ConfigsT $ traverseDMapWithKeyWithAdjust (\k v -> unConfigsT $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = ConfigsT $ traverseIntMapWithKeyWithAdjust (\k v -> unConfigsT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ConfigsT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unConfigsT $ f k v) m e

instance PrimMonad m => PrimMonad (ConfigsT m) where
  type PrimState (ConfigsT m) = PrimState m
  primitive = lift . primitive

runConfigsT
  :: Map Text Text
  -> ConfigsT m a
  -> m a
runConfigsT cs child = runReaderT (unConfigsT child) (Configs cs)

instance Monad m => HasConfigs (ConfigsT m) where
  askConfigs = ConfigsT ask

mapConfigsT
  :: (forall x. m x -> n x)
  -> ConfigsT m a
  -> ConfigsT n a
mapConfigsT f (ConfigsT x) = ConfigsT $ mapReaderT f x

getConfig
  :: HasConfigs m
  => Text
  -> m (Maybe Text)
getConfig key = Map.lookup key . unConfigs <$> askConfigs

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

module Obelisk.Frontend.Cookie where

import Control.Monad.Fix
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Encoding (encodeUtf8)
import Reflex
import Reflex.Host.Class
import Reflex.Dom.Core
import GHCJS.DOM.Document (getCookie, Document)
import GHCJS.DOM.Types (MonadJSM)
import Web.Cookie

import Obelisk.Route.Frontend

class Monad m => HasCookies m where
  askCookies :: m Cookies
  default askCookies :: (HasCookies m', m ~ t m', MonadTrans t) => m Cookies
  askCookies = lift askCookies

instance HasCookies m => HasCookies (BehaviorWriterT t w m)
instance HasCookies m => HasCookies (DynamicWriterT t w m)
instance HasCookies m => HasCookies (EventWriterT t w m)
instance HasCookies m => HasCookies (PostBuildT t m)
instance HasCookies m => HasCookies (QueryT t q m)
instance HasCookies m => HasCookies (ReaderT r m)
instance HasCookies m => HasCookies (RequesterT t request response m)
instance HasCookies m => HasCookies (RouteToUrlT t m)
instance HasCookies m => HasCookies (SetRouteT t r m)
instance HasCookies m => HasCookies (StaticDomBuilderT t m)
instance HasCookies m => HasCookies (TriggerEventT t m)
instance HasCookies m => HasCookies (RoutedT t r m)

newtype CookiesT m a = CookiesT { unCookiesT :: ReaderT Cookies m a }
  deriving
    ( Functor
    , Applicative
    , DomBuilder t
    , Monad
    , MonadFix
    , MonadHold t
    , MonadIO
    , MonadJSM
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

instance Adjustable t m => Adjustable t (CookiesT m) where
  runWithReplace a e = CookiesT $ runWithReplace (unCookiesT a) (unCookiesT <$> e)
  traverseDMapWithKeyWithAdjust f m e = CookiesT $ traverseDMapWithKeyWithAdjust (\k v -> unCookiesT $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = CookiesT $ traverseIntMapWithKeyWithAdjust (\k v -> unCookiesT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = CookiesT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unCookiesT $ f k v) m e

instance PrimMonad m => PrimMonad (CookiesT m) where
  type PrimState (CookiesT m) = PrimState m
  primitive = lift . primitive

runCookiesT
  :: Cookies
  -> CookiesT m a
  -> m a
runCookiesT cs child = runReaderT (unCookiesT child) cs

instance Monad m => HasCookies (CookiesT m) where
  askCookies = CookiesT ask

mapCookiesT
  :: (forall x. m x -> n x)
  -> CookiesT m a
  -> CookiesT n a
mapCookiesT f (CookiesT x) = CookiesT $ mapReaderT f x

instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasCookies (HydrationDomBuilderT s t m) where
  askCookies = fmap (parseCookies . encodeUtf8) $ getCookie =<< askDocument

{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Description:
  Utility and generic functions for getting and setting cookies.

This module provides functions for getting and setting cookies based
on the "Web.Cookie" module. It's recommended to use the 'askCookie' and
'setCookie' functions which Base64 encode their values, because
standard cookies are limited in which characters they can contain.

From <https://www.rfc-editor.org/rfc/rfc6265 RFC 6265: HTTP State Management Mechanism>:

"To maximize compatibility with user agents, servers that wish to
store arbitrary data in a cookie-value SHOULD encode that data, for
example, using Base64"

__Warning__

The 'askCookie' and 'setCookie' functions do not always form a valid State monad. For example, in a 'Snap.Core.Snap' context 'askCookie' examines the HTTP request headers while 'setCookie' changes the outgoing response header.

-}

module Obelisk.Frontend.Cookie
  -- ( HasCookies (..),
  --   CookiesT (..)
  -- )
where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Either.Combinators
import Data.Text (Text)
import Data.Text.Encoding
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import "ghcjs-dom" GHCJS.DOM.Document (Document)
import qualified "ghcjs-dom" GHCJS.DOM.Document as DOM
import GHCJS.DOM.Types (MonadJSM, liftJSM)
import Reflex
import Reflex.Dom.Core
import Reflex.Host.Class
import Web.Cookie

import Obelisk.Configs
import Obelisk.Route.Frontend

-- | Context in which cookies can be read. It's best to use the
-- 'askCookie' function over 'askCookies', in conjunction with Base64 encoded cookies
-- (e.g. via 'setCookie').
--
-- As of writing this class is implemented in the following contexts:
--
-- * Backend, on the 'Snap.Core.Snap' monad where it lists the cookies of the HTTP request.
--
-- * Frontend, on 'Obelisk.Frontend.ObeliskWidget'.
--
-- * Frontend, on 'Reflex.Dom.Core.Client' (this is the @Client m a@ part of 'Reflex.Dom.Prerender.prerender').
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
instance HasCookies m => HasCookies (ConfigsT m)
instance HasConfigs m => HasConfigs (CookiesT m)

-- | Retrieve the value of a Base64 encoded cookie.
askCookie :: (HasCookies m) => Text -> m (Either GetCookieFailed Text)
askCookie key = do
  v <- lookup (encodeUtf8 key) <$> askCookies
  pure $ case v of
    Nothing -> Left GetCookieFailed_NotFound
    Just c -> mapBoth GetCookieFailed_Base64DecodeFailed decodeUtf8 $
      B64.decode c

data GetCookieFailed
  = GetCookieFailed_NotFound
  | GetCookieFailed_Base64DecodeFailed String
  deriving (Eq, Show, Read)

-- | Contexts in which cookies can be set. It's best to use the Base64
-- encoding 'setCookie' function over 'setCookieRaw', because the
-- former doesn't fail silently on unsupported characters. Be warned
-- that browsers still limit the (total) size of cookies. This limit
-- is unchecked.
--
-- As of writing this class is implemented in the following contexts:
--
-- * Backend, on the 'Snap.Core.Snap' monad where it sets cookies for the HTTP response.
--
-- * Frontend, on 'Reflex.Dom.Core.Client' (this is the "Client m a"
--   part of 'Reflex.Dom.Prerender.prerender'). This sets cookies
--   client-side.
class (Monad m) => HasSetCookie m where
  setCookieRaw :: SetCookie -> m ()
  default setCookieRaw :: (HasSetCookie m', m ~ t m', MonadTrans t) => SetCookie -> m ()
  setCookieRaw c = lift (setCookieRaw c)

instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasSetCookie (HydrationDomBuilderT s t m) where
  setCookieRaw cookie = do
    doc <- askDocument
    liftJSM $ DOM.setCookie doc $ decodeUtf8 $ LBS.toStrict $ toLazyByteString $ renderSetCookie cookie


instance (MonadJSM m, HasDocument m, DOM.IsDocument (RawDocument (DomBuilderSpace m))) => HasSetCookie (HydratableT m) where
  setCookieRaw cookie = do
    doc <- askDocument
    liftJSM $ DOM.setCookie doc $ decodeUtf8 $ LBS.toStrict $ toLazyByteString $ renderSetCookie cookie


instance HasSetCookie m => HasSetCookie (BehaviorWriterT t w m)
instance HasSetCookie m => HasSetCookie (DynamicWriterT t w m)
instance HasSetCookie m => HasSetCookie (EventWriterT t w m)
instance HasSetCookie m => HasSetCookie (PostBuildT t m)
instance HasSetCookie m => HasSetCookie (QueryT t q m)
instance HasSetCookie m => HasSetCookie (ReaderT r m)
instance HasSetCookie m => HasSetCookie (RequesterT t request response m)
instance HasSetCookie m => HasSetCookie (RouteToUrlT t m)
instance HasSetCookie m => HasSetCookie (SetRouteT t r m)
instance HasSetCookie m => HasSetCookie (StaticDomBuilderT t m)
instance HasSetCookie m => HasSetCookie (TriggerEventT t m)
instance HasSetCookie m => HasSetCookie (RoutedT t r m)
instance HasSetCookie m => HasSetCookie (ConfigsT m)
instance HasSetCookie m => HasSetCookie (CookiesT m)
instance (MonadJSM m, RawDocument (DomBuilderSpace (HydrationDomBuilderT s t m)) ~ Document) => HasCookies (HydrationDomBuilderT s t m) where
  askCookies = fmap (parseCookies . encodeUtf8) $ DOM.getCookie =<< askDocument

instance (MonadJSM m, HasDocument m, DOM.IsDocument (RawDocument (DomBuilderSpace m))) => HasCookies (HydratableT m) where
  askCookies = fmap (parseCookies . encodeUtf8) $ DOM.getCookie =<< askDocument

-- | Store a cookie which will be Base64 encoded.
setCookie :: (HasSetCookie m) => SetCookie -> m ()
setCookie = setCookieRaw . (\c -> c { setCookieValue = B64.encode (setCookieValue c) })



-- TODO: Make generic over both frontend and backend.
-- | Make a cookie with sensible defaults.
defaultCookie
  :: (MonadJSM m)
  => Text  -- ^ Cookie key.
  -> Maybe Text  -- ^ Cookie value ('Nothing' clears it).
  -> m SetCookie
defaultCookie key mv = do
  currentProtocol <- Reflex.Dom.Core.getLocationProtocol
  pure $ case mv of
    Nothing -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = ""
      , setCookieExpires = Just $ posixSecondsToUTCTime 0
      }
    Just val -> def
      { setCookieName = encodeUtf8 key
      , setCookieValue = encodeUtf8 val
      -- We don't want these to expire, but browsers don't support
      -- non-expiring cookies.  Some systems have trouble representing dates
      -- past 2038, so use 2037.
      , setCookieExpires = Just $ UTCTime (fromGregorian 2037 1 1) 0
      , setCookieSecure = currentProtocol == "https:"
      -- This helps prevent CSRF attacks; we don't want strict, because it
      -- would prevent links to the page from working; lax is secure enough,
      -- because we don't take dangerous actions simply by executing a GET
      -- request.
      , setCookieSameSite = if currentProtocol == "file:"
          then Nothing
          else Just sameSiteLax
      }

-- | Clear a cookie.
clearCookie :: (HasSetCookie m)
  => Text -- ^ Cookie key
  -> m ()
clearCookie key = do
  setCookie (def { setCookieName = encodeUtf8 key
                 , setCookieValue = ""
                 , setCookieExpires = Just $ posixSecondsToUTCTime 0
                 })

-- | A trivial implementation of 'HasCookies' as a Reader monad.
newtype CookiesT m a = CookiesT { unCookiesT :: ReaderT Cookies m a }
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
    , Prerender t
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


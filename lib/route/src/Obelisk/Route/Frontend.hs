{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Obelisk.Route.Frontend
  ( module Obelisk.Route
  , pattern (:~)
  , Routed
  , RoutedT
  , runRoutedT
  , askRoute
  , withRoutedT
  , mapRoutedT
  , subRoute
  , subRoute_
  , subPairRoute
  , subPairRoute_
  , maybeRoute
  , maybeRoute_
  , maybeRouted
  , eitherRoute
  , eitherRoute_
  , eitherRouted
  , runRouteViewT
  , SetRouteT(..)
  , SetRoute(..)
  , runSetRouteT
  , mapSetRouteT
  , RouteToUrl(..)
  , RouteToUrlT(..)
  , runRouteToUrlT
  , mapRouteToUrlT
  , routeLink
  , routeLinkDynAttr
  , dynRouteLink
  , adaptedUriPath
  , setAdaptedUriPath
  ) where

import Prelude hiding ((.), id)

import Control.Category (Category (..), (.))
import Control.Category.Cartesian ((&&&))
import Control.Lens hiding (Bifunctor, bimap, universe, element)
import Control.Monad ((<=<))
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Control
import Data.Coerce
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Compose
import Data.Functor.Misc
import Data.GADT.Compare
import qualified Data.List as L
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Data.Type.Coercion
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Window as Window
import Language.Javascript.JSaddle (MonadJSM, jsNull, liftJSM) --TODO: Get rid of this - other platforms can also be routed
import Network.URI
import Reflex.Class
import Reflex.Dom.Builder.Class
import Reflex.Dom.Core
import Reflex.Host.Class
import Unsafe.Coerce

import Obelisk.Configs
import Obelisk.Route

infixr 5 :~
pattern (:~) :: Reflex t => f a -> Dynamic t a -> DSum f (Compose (Dynamic t) Identity)
pattern a :~ b <- a :=> (coerceDynamic . getCompose -> b)

class Routed t r m | m -> t r where
  askRoute :: m (Dynamic t r)
  default askRoute :: (Monad m', MonadTrans f, Routed t r m', m ~ f m') => m (Dynamic t r)
  askRoute = lift askRoute

instance Monad m => Routed t r (RoutedT t r m) where
  askRoute = RoutedT ask

instance (Monad m, Routed t r m) => Routed t r (ReaderT r' m)

newtype RoutedT t r m a = RoutedT { unRoutedT :: ReaderT (Dynamic t r) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadTrans
    , MFunctor
    , NotReady t
    , MonadHold t
    , MonadSample t
    , PostBuild t
    , TriggerEvent t
    , MonadIO
    , MonadReflexCreateTrigger t
    , HasDocument
    , DomRenderHook t
    )

instance MonadReader r' m => MonadReader r' (RoutedT t r m) where
  ask = lift ask
  local = mapRoutedT . local

instance (Prerender t m, Monad m) => Prerender t (RoutedT t r m) where
  type Client (RoutedT t r m) = RoutedT t r (Client m)
  prerender server client = RoutedT $ do
    r <- ask
    lift $ prerender (runRoutedT server r) (runRoutedT client r)

instance Requester t m => Requester t (RoutedT t r m) where
  type Request (RoutedT t r m) = Request m
  type Response (RoutedT t r m) = Response m
  requesting = RoutedT . requesting
  requesting_ = RoutedT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (RoutedT t r m)
#endif

instance PerformEvent t m => PerformEvent t (RoutedT t r m) where
  type Performable (RoutedT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (RoutedT t r m) where
  type Ref (RoutedT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

deriving instance EventWriter t w m => EventWriter t w (RoutedT t r m)

instance MonadTransControl (RoutedT t r) where
  type StT (RoutedT t r) a = StT (ReaderT (Dynamic t r)) a
  liftWith = defaultLiftWith RoutedT unRoutedT
  restoreT = defaultRestoreT RoutedT

instance PrimMonad m => PrimMonad (RoutedT t r m ) where
  type PrimState (RoutedT t r m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (RoutedT t r m) where
  type DomBuilderSpace (RoutedT t r m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (RoutedT t r m) where
  runWithReplace a0 a' = RoutedT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (RoutedT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance HasConfigs m => HasConfigs (RoutedT t r m)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (QueryT t q m)

instance (Monad m, SetRoute t r m) => SetRoute t r (QueryT t q m)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (EventWriterT t w m)

instance (Monad m, SetRoute t r m) => SetRoute t r (EventWriterT t w m)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (DynamicWriterT t w m)

instance (Monad m, SetRoute t r m) => SetRoute t r (DynamicWriterT t w m)

runRoutedT :: RoutedT t r m a -> Dynamic t r -> m a
runRoutedT = runReaderT . unRoutedT

mapRoutedT :: (m a -> n b) -> RoutedT t r m a -> RoutedT t r n b
mapRoutedT f = RoutedT . mapReaderT f . unRoutedT

withRoutedT :: (Dynamic t r -> Dynamic t r') -> RoutedT t r' m a -> RoutedT t r m a
withRoutedT f = RoutedT . withReaderT f . unRoutedT

subRoute_ :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m ()) -> RoutedT t (R r) m ()
subRoute_ f = factorRouted $ strictDynWidget_ $ \(c :=> r') -> do
  runRoutedT (f c) r'

-- | Like 'subRoute_', but with a pair rather than an R
subPairRoute_ :: (MonadFix m, MonadHold t m, Eq a, Adjustable t m) => (a -> RoutedT t b m ()) -> RoutedT t (a, b) m ()
subPairRoute_ f = withRoutedT (fmap (\(a, b) -> Const2 a :/ b)) $ subRoute_ (\(Const2 a) -> f a)

subRoute :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m b) -> RoutedT t (R r) m (Dynamic t b)
subRoute f = factorRouted $ strictDynWidget $ \(c :=> r') -> do
  runRoutedT (f c) r'

-- | Like 'subRoute_', but with a pair rather than an R
subPairRoute :: (MonadFix m, MonadHold t m, Eq a, Adjustable t m) => (a -> RoutedT t b m c) -> RoutedT t (a, b) m (Dynamic t c)
subPairRoute f = withRoutedT (fmap (\(a, b) -> Const2 a :/ b)) $ subRoute (\(Const2 a) -> f a)

maybeRoute_ :: (MonadFix m, MonadHold t m, Adjustable t m) => m () -> RoutedT t r m () -> RoutedT t (Maybe r) m ()
maybeRoute_ n j = maybeRouted $ strictDynWidget_ $ \case
  Nothing -> n
  Just r -> runRoutedT j r

maybeRoute :: (MonadFix m, MonadHold t m, Adjustable t m) => m a -> RoutedT t r m a -> RoutedT t (Maybe r) m (Dynamic t a)
maybeRoute n j = maybeRouted $ strictDynWidget $ \case
  Nothing -> n
  Just r -> runRoutedT j r

{-
maybeRoute :: (MonadFix m, MonadHold t m, GEq r, Adjustable t m) => m a -> RoutedT t r m a -> RoutedT t (Maybe r) m a
maybeRoute f = factorRouted $ strictDynWidget $ \(c :=> r') -> do
  runRoutedT (f c) r'
-}

eitherRoute_
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t l m ()
  -> RoutedT t r m ()
  -> RoutedT t (Either l r) m ()
eitherRoute_ l r = eitherRouted $ strictDynWidget_ $ either (runRoutedT l) (runRoutedT r)

eitherRoute
  :: (MonadFix m, MonadHold t m, Adjustable t m)
  => RoutedT t l m a
  -> RoutedT t r m a
  -> RoutedT t (Either l r) m (Dynamic t a)
eitherRoute l r = eitherRouted $ strictDynWidget $ either (runRoutedT l) (runRoutedT r)

dsumValueCoercion :: Coercion f g -> Coercion (DSum k f) (DSum k g)
dsumValueCoercion Coercion = Coercion

dynamicIdentityCoercion :: Coercion (Compose (Dynamic t) Identity) (Dynamic t)
dynamicIdentityCoercion = unsafeCoerce (Coercion :: Coercion (Identity ()) ()) --TODO: Is it possible to prove this?

factorRouted :: (Reflex t, MonadFix m, MonadHold t m, GEq f) => RoutedT t (DSum f (Dynamic t)) m a -> RoutedT t (DSum f Identity) m a
factorRouted r = RoutedT $ ReaderT $ \d -> do
  d' <- factorDyn d
  runRoutedT r $ coerceWith (dynamicCoercion $ dsumValueCoercion dynamicIdentityCoercion) d'

maybeRouted :: (Reflex t, MonadFix m, MonadHold t m) => RoutedT t (Maybe (Dynamic t a)) m b -> RoutedT t (Maybe a) m b
maybeRouted r = RoutedT $ ReaderT $ \d -> do
  d' <- maybeDyn d
  runRoutedT r d'

eitherRouted :: (Reflex t, MonadFix m, MonadHold t m) => RoutedT t (Either (Dynamic t a) (Dynamic t b)) m c -> RoutedT t (Either a b) m c
eitherRouted r = RoutedT $ ReaderT $ runRoutedT r <=< eitherDyn

-- | WARNING: The input 'Dynamic' must be fully constructed when this is run
strictDynWidget :: (MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m b) -> RoutedT t a m (Dynamic t b)
strictDynWidget f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (result0, result') <- runWithReplace (f r0) $ f <$> updated r
  holdDyn result0 result'

strictDynWidget_ :: (MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m ()) -> RoutedT t a m ()
strictDynWidget_ f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (_, _) <- runWithReplace (f r0) $ f <$> updated r
  pure ()

-- Semigroup.Any but with more helpful names.
data Elision = Kept | Elided
instance Semigroup Elision where
  Elided <> a = a
  Kept <> _ = Kept

newtype SetRouteT t r m a = SetRouteT { unSetRouteT :: EventWriterT t (Elision, Endo r) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

instance (MonadFix m, MonadHold t m, DomBuilder t m) => DomBuilder t (SetRouteT t r m) where
  type DomBuilderSpace (SetRouteT t r m) = DomBuilderSpace m
  element t cfg child = SetRouteT $ element t cfg $ unSetRouteT child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg child = SetRouteT $ selectElement cfg $ unSetRouteT child

mapSetRouteT :: (forall x. m x -> n x) -> SetRouteT t r m a -> SetRouteT t r n a
mapSetRouteT f (SetRouteT x) = SetRouteT (mapEventWriterT f x)

runSetRouteT :: (Reflex t, Monad m) => SetRouteT t r m a -> m (a, Event t (Elision, Endo r))
runSetRouteT = runEventWriterT . unSetRouteT

class Reflex t => SetRoute t r m | m -> t r where
  setRoute :: Event t r -> m ()
  modifyRoute :: Event t (r -> r) -> m ()
  default modifyRoute :: (Monad m', MonadTrans f, SetRoute t r m', m ~ f m') => Event t (r -> r) -> m ()
  modifyRoute = lift . modifyRoute

  setRoute = modifyRoute . fmap const

  replaceRoute :: Event t r -> m ()
  default replaceRoute :: (Monad m', MonadTrans f, SetRoute t r m', m ~ f m') => Event t r -> m ()
  replaceRoute = lift . replaceRoute

instance (Reflex t, Monad m) => SetRoute t r (SetRouteT t r m) where
  modifyRoute = SetRouteT . tellEvent . fmap ((,) Kept . Endo)
  replaceRoute = SetRouteT . tellEvent . fmap ((,) Elided . Endo . const)

instance (Monad m, SetRoute t r m) => SetRoute t r (RoutedT t r' m)

instance (Monad m, SetRoute t r m) => SetRoute t r (ReaderT r' m)

instance (PerformEvent t m, Prerender t m, Monad m, Reflex t) => Prerender t (SetRouteT t r m) where
  type Client (SetRouteT t r m) = SetRouteT t r (Client m)
  prerender server client = do
    d <- lift $ prerender (runSetRouteT server) (runSetRouteT client)
    let (a, r) = splitDynPure d
    -- Must be prompt here
    SetRouteT . tellEvent $ switchPromptlyDyn r
    pure a

instance Requester t m => Requester t (SetRouteT t r m) where
  type Request (SetRouteT t r m) = Request m
  type Response (SetRouteT t r m) = Response m
  requesting = SetRouteT . requesting
  requesting_ = SetRouteT . requesting_

instance (Monad m, SetRoute t r m) => SetRoute t r (RequesterT t req rsp m)

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SetRouteT t r m)
#endif

instance PerformEvent t m => PerformEvent t (SetRouteT t r m) where
  type Performable (SetRouteT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (SetRouteT t r m) where
  type Ref (SetRouteT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance PrimMonad m => PrimMonad (SetRouteT t r m ) where
  type PrimState (SetRouteT t r m) = PrimState m
  primitive = lift . primitive

instance HasConfigs m => HasConfigs (SetRouteT t r m)

instance (MonadHold t m, Adjustable t m) => Adjustable t (SetRouteT t r m) where
  runWithReplace a0 a' = SetRouteT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (SetRouteT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

class RouteToUrl r m | m -> r where
  askRouteToUrl :: m (r -> Text)
  default askRouteToUrl :: (Monad m', MonadTrans f, RouteToUrl r m', m ~ f m') => m (r -> Text)
  askRouteToUrl = lift askRouteToUrl

newtype RouteToUrlT r m a = RouteToUrlT { unRouteToUrlT :: ReaderT (r -> Text) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadIO, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

runRouteToUrlT
  :: RouteToUrlT r m a
  -> (r -> Text)
  -> m a
runRouteToUrlT a = runReaderT (unRouteToUrlT a)

mapRouteToUrlT :: (forall x. m x -> n x) -> RouteToUrlT r m a -> RouteToUrlT r n a
mapRouteToUrlT f (RouteToUrlT m) = RouteToUrlT $ mapReaderT f m

instance Monad m => RouteToUrl r (RouteToUrlT r m) where
  askRouteToUrl = RouteToUrlT ask

instance (Monad m, RouteToUrl r m) => RouteToUrl r (SetRouteT t r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (RoutedT t r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (ReaderT r' m) where

instance (Monad m, RouteToUrl r m) => RouteToUrl r (RequesterT t req rsp m)

instance (Prerender t m, Monad m) => Prerender t (RouteToUrlT r m) where
  type Client (RouteToUrlT r m) = RouteToUrlT r (Client m)
  prerender server client = do
    r <- RouteToUrlT ask
    lift $ prerender (runRouteToUrlT server r) (runRouteToUrlT client r)

instance Requester t m => Requester t (RouteToUrlT r m) where
  type Request (RouteToUrlT r m) = Request m
  type Response (RouteToUrlT r m) = Response m
  requesting = RouteToUrlT . requesting
  requesting_ = RouteToUrlT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (RouteToUrlT r m)
#endif

instance PerformEvent t m => PerformEvent t (RouteToUrlT r m) where
  type Performable (RouteToUrlT r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (RouteToUrlT r m) where
  type Ref (RouteToUrlT r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadTransControl (RouteToUrlT r) where
  type StT (RouteToUrlT r) a = StT (ReaderT (r -> Text)) a
  liftWith = defaultLiftWith RouteToUrlT unRouteToUrlT
  restoreT = defaultRestoreT RouteToUrlT

instance PrimMonad m => PrimMonad (RouteToUrlT r m ) where
  type PrimState (RouteToUrlT r m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (RouteToUrlT r m) where
  type DomBuilderSpace (RouteToUrlT r m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (RouteToUrlT r m) where
  runWithReplace a0 a' = RouteToUrlT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RouteToUrlT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RouteToUrlT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RouteToUrlT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (RouteToUrlT r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

instance HasConfigs m => HasConfigs (RouteToUrlT t m)

runRouteViewT
  :: forall t m r a.
     ( TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     )
  => Encoder Identity Identity r PageName
  --TODO: Get rid of the switchover and useHash arguments
  -- useHash can probably be baked into the encoder
  -> Event t () -- ^ Switchover event, nothing is done until this event fires. Used to prevent incorrect DOM expectations at hydration switchover time
  -> Bool
  -> RoutedT t r (SetRouteT t r (RouteToUrlT r m)) a
  -> m a
runRouteViewT routeEncoder switchover useHash a = do
  rec historyState <- manageHistory' switchover $ setState
      let theEncoder = pageNameEncoder . hoistParse (pure . runIdentity) routeEncoder
          -- NB: The can only fail if the uriPath doesn't begin with a '/' or if the uriQuery
          -- is nonempty, but begins with a character that isn't '?'. Since we don't expect
          -- this ever to happen, we'll just handle it by failing completely with 'error'.
          route :: Dynamic t r
          route = fmap (errorLeft . tryDecode theEncoder . (adaptedUriPath useHash &&& uriQuery) . _historyItem_uri) historyState
            where
              errorLeft (Left e) = error (T.unpack e)
              errorLeft (Right x) = x
      (result, changeState) <- runRouteToUrlT (runSetRouteT $ runRoutedT a route) $ (\(p, q) -> T.pack $ p <> q) . encode theEncoder
      let f (currentHistoryState, oldRoute) (elision, change) =
            let newRoute = appEndo change oldRoute
                (newPath, newQuery) = encode theEncoder newRoute
                historyUpdate = HistoryStateUpdate
                  { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull
                    -- We always provide "" as the title.  On Firefox, Chrome, and
                    -- Edge, this parameter does nothing.  On Safari, "" has the
                    -- same behavior as other browsers (as far as I can tell), but
                    -- anything else sets the title for the back button list item
                    -- the *next* time pushState is called, unless the page title
                    -- is changed in the interim.  Since the Safari functionality
                    -- is near-pointless and also confusing, I'm not going to even
                    -- bother exposing it; if there ends up being a real use case,
                    -- we can change this function later to accommodate.
                    -- See: https://github.com/whatwg/html/issues/2174
                  , _historyStateUpdate_title = ""
                  , _historyStateUpdate_uri = Just $ setAdaptedUriPath useHash newPath $ (_historyItem_uri currentHistoryState)
                    { uriQuery = newQuery
                    }
                  }
                in case elision of
                  Kept -> HistoryCommand_PushState historyUpdate
                  Elided -> HistoryCommand_ReplaceState historyUpdate
          setState = attachWith f ((,) <$> current historyState <*> current route) changeState
  return result

-- | A link widget that, when clicked, sets the route to the provided route. In non-javascript
-- contexts, this widget falls back to using @href@s to control navigation
routeLink
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     , Prerender t m
     )
  => route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
routeLink r w = do
  (e, a) <- routeLinkImpl r w
  scrollToTop e
  return a

-- | Raw implementation of 'routeLink'. Does not scroll to the top of the page on clicks.
routeLinkImpl
  :: forall t m a route.
     ( DomBuilder t m
     , RouteToUrl route m
     , SetRoute t route m
     )
  => route -- ^ Target route
  -> m a -- ^ Child widget
  -> m (Event t (), a)
routeLinkImpl r w = do
  enc <- askRouteToUrl
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ "href" =: enc r
  (e, a) <- element "a" cfg w
  setRoute $ r <$ domEvent Click e
  return (domEvent Click e, a)

scrollToTop :: forall m t. (Prerender t m, Monad m) => Event t () -> m ()
scrollToTop e = prerender_ blank $ performEvent_ $ ffor e $ \_ -> liftJSM $ DOM.currentWindow >>= \case
  Nothing -> pure ()
  Just win -> Window.scrollTo win 0 0

-- | Like 'routeLinkDynAttr' but without custom attributes.
dynRouteLink
  :: forall t m a route.
     ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl route m
     , SetRoute t route m
     , Prerender t m
     )
  => Dynamic t route -- ^ Target route
  -> m a -- ^ Child widget
  -> m a
dynRouteLink r w = do
  (e, a) <- dynRouteLinkImpl r w
  scrollToTop e
  return a

-- | Raw implementation of 'dynRouteLink'. Does not scroll to the top of the page on clicks.
dynRouteLinkImpl
  :: forall t m a route.
     ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl route m
     , SetRoute t route m
     )
  => Dynamic t route -- ^ Target route
  -> m a -- ^ Child widget
  -> m (Event t (), a)
dynRouteLinkImpl dr w = do
  enc <- askRouteToUrl
  er <- dynamicAttributesToModifyAttributes $ ("href" =:) . enc <$> dr
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ er
  (e, a) <- element "a" cfg w
  let clk = domEvent Click e
  setRoute $ tag (current dr) clk
  return (clk, a)

-- | An @a@-tag link widget that, when clicked, sets the route to current value of the
-- provided dynamic route. In non-JavaScript contexts the value of the dynamic post
-- build is used so the link still works like 'routeLink'.
routeLinkDynAttr
  :: forall t m a route.
     ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     , Prerender t m
     )
  => Dynamic t (Map AttributeName Text) -- ^ Attributes for @a@ element. Note that if @href@ is present it will be ignored
  -> Dynamic t (R route) -- ^ Target route
  -> m a -- ^ Child widget of the @a@ element
  -> m a
routeLinkDynAttr dAttr dr w = do
  (e, a) <- routeLinkDynAttrImpl dAttr dr w
  scrollToTop e
  return a

-- | Raw implementation of 'routeLinkDynAttr'. Does not scroll to the top of the page on clicks.
routeLinkDynAttrImpl
  :: forall t m a route.
     ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R route) m
     , SetRoute t (R route) m
     )
  => Dynamic t (Map AttributeName Text) -- ^ Attributes for @a@ element. Note that if @href@ is present it will be ignored
  -> Dynamic t (R route) -- ^ Target route
  -> m a -- ^ Child widget of the @a@ element
  -> m (Event t (), a)
routeLinkDynAttrImpl dAttr dr w = do
  enc <- askRouteToUrl
  er <- dynamicAttributesToModifyAttributes $ zipDynWith (<>) (("href" =:) . enc <$> dr) dAttr
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ er
  (e, a) <- element "a" cfg w
  let clk = domEvent Click e
  setRoute $ tag (current dr) clk
  return (clk, a)

-- On ios due to sandboxing when loading the page from a file adapt the
-- path to be based on the hash.

adaptedUriPath :: Bool -> URI -> String
adaptedUriPath = \case
  True -> hashToPath . uriFragment
  False -> uriPath

setAdaptedUriPath :: Bool -> String -> URI -> URI
setAdaptedUriPath useHash s u = case useHash of
  True -> u { uriFragment = pathToHash s }
  False -> u { uriPath = s }

pathToHash :: String -> String
pathToHash = ('#' :) . fromMaybe "" . L.stripPrefix "/"

hashToPath :: String -> String
hashToPath = ('/' :) . fromMaybe "" . L.stripPrefix "#"

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
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
  , runRouteViewT
  ) where

import Prelude hiding ((.), id)

import Obelisk.Route

import Control.Category (Category (..), (.))
import Control.Category.Cartesian
import Control.Lens hiding (Bifunctor, bimap, universe)
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Dependent.Sum (DSum (..))
import Data.GADT.Compare
import Data.GADT.Show
import Data.Monoid
import Data.Some (Some)
import Data.Text (Text)
import Data.Universe
import Data.Functor.Compose
import Reflex.Class
import Reflex.PostBuild.Class
import Reflex.TriggerEvent.Class
import Reflex.PerformEvent.Class
import Reflex.EventWriter
import Reflex.Dynamic
import Reflex.Dom.Builder.Class
import Data.Type.Coercion
import Language.Javascript.JSaddle --TODO: Get rid of this - other platforms can also be routed
import Reflex.Dom.Location
import qualified GHCJS.DOM.Types as DOM
import Network.URI

import Unsafe.Coerce

infixr 5 :~
pattern (:~) :: Reflex t => f a -> Dynamic t a -> DSum f (Compose (Dynamic t) Identity)
pattern a :~ b <- a :=> (coerceDynamic . getCompose -> b)

class Routed t r m where
  askRoute :: m (Dynamic t r)

instance Monad m => Routed t r (RoutedT t r m) where
  askRoute = RoutedT ask

newtype RoutedT t r m a = RoutedT { unRoutedT :: ReaderT (Dynamic t r) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t)

deriving instance EventWriter t w m => EventWriter t w (RoutedT t r m)

instance MonadTransControl (RoutedT t r) where
  type StT (RoutedT t r) a = StT (ReaderT (Dynamic t r)) a
  liftWith = defaultLiftWith RoutedT unRoutedT
  restoreT = defaultRestoreT RoutedT

instance DomBuilder t m => DomBuilder t (RoutedT t r m) where
  type DomBuilderSpace (RoutedT t r m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (RoutedT t r m) where
  runWithReplace a0 a' = RoutedT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = RoutedT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

runRoutedT :: RoutedT t r m a -> Dynamic t r -> m a
runRoutedT = runReaderT . unRoutedT

mapRoutedT :: (m a -> n a) -> RoutedT t r m a -> RoutedT t r n a
mapRoutedT f = RoutedT . mapReaderT f . unRoutedT

withRoutedT :: (Dynamic t r -> Dynamic t r') -> RoutedT t r' m a -> RoutedT t r m a
withRoutedT f = RoutedT . withReaderT f . unRoutedT

subRoute_ :: (Reflex t, MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m ()) -> RoutedT t (R r) m ()
subRoute_ f = factorRouted $ strictDynWidget_ $ \(c :=> r') -> do
  runRoutedT (f c) r'

subRoute :: (Reflex t, MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m b) -> RoutedT t (R r) m (Dynamic t b)
subRoute f = factorRouted $ strictDynWidget $ \(c :=> r') -> do
  runRoutedT (f c) r'

dsumValueCoercion :: Coercion f g -> Coercion (DSum k f) (DSum k g)
dsumValueCoercion Coercion = Coercion

dynamicIdentityCoercion :: Coercion (Compose (Dynamic t) Identity) (Dynamic t)
dynamicIdentityCoercion = unsafeCoerce (Coercion :: Coercion (Identity ()) ()) --TODO: Is it possible to prove this?

factorRouted :: (Reflex t, MonadFix m, MonadHold t m, GEq f) => RoutedT t (DSum f (Dynamic t)) m a -> RoutedT t (DSum f Identity) m a
factorRouted r = RoutedT $ ReaderT $ \d -> do
  d' <- factorDyn d
  runRoutedT r $ (coerceWith (dynamicCoercion $ dsumValueCoercion dynamicIdentityCoercion) d')

-- | WARNING: The input 'Dynamic' must be fully constructed when this is run
strictDynWidget :: (Reflex t, MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m b) -> RoutedT t a m (Dynamic t b)
strictDynWidget f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (result0, result') <- runWithReplace (f r0) $ f <$> updated r
  holdDyn result0 result'

strictDynWidget_ :: (Reflex t, MonadSample t m, MonadHold t m, Adjustable t m) => (a -> m ()) -> RoutedT t a m ()
strictDynWidget_ f = RoutedT $ ReaderT $ \r -> do
  r0 <- sample $ current r
  (_, _) <- runWithReplace (f r0) $ f <$> updated r
  pure ()

--TODO: Factor this out into Obelisk
runRouteViewT
  :: forall t m r a.
     ( Monad m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , MonadFix m
     , Adjustable t m
     , DomBuilder t m
     , Universe (Some r)
     , GCompare r
     , GShow r
     )
  => (Encoder (Either Text) (Either Text) (Some r) (Maybe Text))
  -> (forall b. r b -> Encoder (Either Text) (Either Text) b PageName)
  -> (R r -> Text)
  -> (Text -> R r) -- ^ 404 page
  -> RoutedT t (R r) (EventWriterT t (Endo (R r)) m) a
  -> m a
runRouteViewT routeComponentEncoder routeRestEncoder routeToTitle error404 a = do
  rec historyState <- manageHistory $ HistoryCommand_PushState <$> setState
      let Right myEncoder = checkEncoder $ obeliskRouteEncoder routeComponentEncoder routeRestEncoder . Encoder (pure $ prismValidEncoder $ rPrism _ObeliskRoute_App)
          route :: Dynamic t (R r)
          route = fmap (runIdentity . _validEncoder_decode (catchValidEncoder error404 $ pageNameValidEncoder . myEncoder) . (uriPath &&& uriQuery) . _historyItem_uri) historyState
      (result, changeState) <- runEventWriterT $ runRoutedT a route
      let f oldRoute change =
            let newRoute = appEndo change oldRoute
                (newPath, newQuery) = _validEncoder_encode (pageNameValidEncoder . myEncoder) newRoute
            in HistoryStateUpdate
               { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull
               , _historyStateUpdate_title = routeToTitle newRoute
               , _historyStateUpdate_uri = Just $ nullURI
                 { uriPath = newPath
                 , uriQuery = newQuery
                 }
               }
          setState = attachWith f (current route) changeState
  return result

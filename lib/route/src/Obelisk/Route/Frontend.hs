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
  ( pattern (:~)
  , Routed
  , RoutedT
  , runRoutedT
  , askRoute
  , withRoutedT
  , mapRoutedT
  , subRoute_
  , runRouteViewT
  ) where

import Prelude hiding ((.), id)

import Obelisk.Route

import Control.Applicative
import Control.Category (Category (..), (.), id)
import qualified Control.Categorical.Functor as Cat
import Control.Categorical.Bifunctor
import Control.Category.Cartesian
import Control.Lens hiding (Bifunctor, bimap, universe)
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Trans.Control
import Control.Monad.Trans.Reader
import Data.Coerce
import Data.Dependent.Sum (DSum (..), ShowTag (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either.Validation (Validation (..))
import Data.Universe
import Data.Functor.Compose
import Data.Functor.Identity
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

--TODO: This needs to use a monad other than ReaderT - something that guarantees that the value is actually available.  Perhaps a "strict ReaderT"? Otherwise factorDyn will fail
subRoute_ :: (Reflex t, MonadFix m, MonadHold t m, GEq r, Adjustable t m) => (forall a. r a -> RoutedT t a m ()) -> RoutedT t (R r) m ()
subRoute_ f = factorRouted $ strictDynWidget_ $ \(c :=> r') -> do
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
runRouteViewT routeEncoder routeRestEncoder routeToTitle error404 a = do
  rec historyState <- manageHistory $ HistoryCommand_PushState <$> setState
      let Right myEncoder = checkEncoder $ obeliskRouteEncoder routeEncoder routeRestEncoder . Encoder (pure $ prismValidEncoder $ rPrism _ObeliskRoute_App)
          route :: Dynamic t (R r)
          route = fmap (runIdentity . _validEncoder_decode (catchValidEncoder error404 $ pageNameEncoder . myEncoder) . (uriPath &&& uriQuery) . _historyItem_uri) historyState
      (result, changeState) <- runEventWriterT $ runRoutedT a route
      let f oldRoute change =
            let newRoute = appEndo change oldRoute
                (newPath, newQuery) = _validEncoder_encode (pageNameEncoder . myEncoder) newRoute
            in HistoryStateUpdate
               { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull
               , _historyStateUpdate_title = "Reflex FRP - " <> routeToTitle newRoute
               , _historyStateUpdate_uri = Just $ nullURI
                 { uriPath = newPath
                 , uriQuery = newQuery
                 }
               }
          setState = attachWith f (current route) changeState
  return result

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  , runFrontend
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Sum
import Data.IORef
import Data.Monoid hiding (Sum)
import Data.Text (Text)
import GHCJS.DOM hiding (bracket, catch)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle (JSM)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import qualified Reflex.TriggerEvent.Base as TriggerEvent

makePrisms ''Sum

type ObeliskWidget t route m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , TriggerEvent t m
--  , HasDocument m --TODO: Would need StaticDomBuilderT to have this
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , EventWriter t (Endo route) m
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  , _frontend_body :: !(forall t m x. (MonadWidget t m, HasJS x m, EventWriter t (Endo route) m) => RoutedT t route m ())
  , _frontend_title :: !(route -> Text)
  , _frontend_notFoundRoute :: !(Text -> route) --TODO: Instead, maybe we should just require that the `parse` Monad for routeEncoder be `Identity`
  }

type Widget' x = ImmediateDomBuilderT DomTimeline (DomCoreWidget x)

-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget x = TriggerEventT DomTimeline (DomCoreWidget x)

type DomCoreWidget x = PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))

--TODO: Upstream
instance HasJS x m => HasJS x (EventWriterT t w m) where
  type JSX (EventWriterT t w m) = JSX m
  liftJS = lift . liftJS

--TODO: Rename
{-# INLINABLE attachWidget''' #-}
attachWidget'''
  :: (EventChannel -> PerformEventT DomTimeline (SpiderHost Global) (IORef (Maybe (EventTrigger DomTimeline ()))))
  -> IO ()
attachWidget''' w = runDomHost $ do
  events <- liftIO newChan
  (postBuildTriggerRef, fc@(FireCommand fire)) <- hostPerformEventT $ w events
  mPostBuildTrigger <- readRef postBuildTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
  liftIO $ processAsyncEvents events fc

--TODO: This is a collection of random stuff; we should make it make some more sense and then upstream to reflex-dom-core
runWithHeadAndBody
  :: (   (forall c. Widget' () c -> FloatingWidget () c) -- "Append to head"
      -> (forall c. Widget' () c -> FloatingWidget () c) -- "Append to body"
      -> FloatingWidget () ()
     )
  -> JSM ()
runWithHeadAndBody app = withJSContextSingletonMono $ \jsSing -> do
  globalDoc <- currentDocumentUnchecked
  headFragment <- createDocumentFragment globalDoc
  bodyFragment <- createDocumentFragment globalDoc
  unreadyChildren <- liftIO $ newIORef 0
  let commit = do
        headElement <- getHeadUnchecked globalDoc
        replaceElementContents headElement headFragment
        bodyElement <- getBodyUnchecked globalDoc
        replaceElementContents bodyElement bodyFragment
  liftIO $ attachWidget''' $ \events -> flip runWithJSContextSingleton jsSing $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let appendImmediateDom :: DOM.DocumentFragment -> Widget' () c -> FloatingWidget () c
        appendImmediateDom df w = do
          events' <- TriggerEvent.askEvents
          lift $ do
            doc <- getOwnerDocumentUnchecked df
            let builderEnv = ImmediateDomBuilderEnv
                  { _immediateDomBuilderEnv_document = doc
                  , _immediateDomBuilderEnv_parent = toNode df
                  , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
                  , _immediateDomBuilderEnv_commitAction = commit
                  }
            runImmediateDomBuilderT w builderEnv events'
    flip runPostBuildT postBuild $ flip runTriggerEventT events $ app (appendImmediateDom headFragment) (appendImmediateDom bodyFragment)
    liftIO (readIORef unreadyChildren) >>= \case
      0 -> DOM.liftJSM commit
      _ -> return ()
    return postBuildTriggerRef

runFrontend :: forall backendRoute route. ValidEncoder (Either Text) (R (Sum backendRoute (ObeliskRoute route))) PageName -> Frontend (R route) -> JSM ()
runFrontend validFullEncoder frontend = do
  let ve = validFullEncoder . prismValidEncoder (rPrism $ _InR . _ObeliskRoute_App)
      runMyRouteViewT
        :: ( TriggerEvent t m
           , PerformEvent t m
           , MonadHold t m
           , DOM.MonadJSM m
           , DOM.MonadJSM (Performable m)
           , MonadFix m
           )
        => RoutedT t (R route) (EventWriterT t (Endo (R route)) m) a
        -> m a
      runMyRouteViewT = runRouteViewT
        ve
        (_frontend_title frontend)
        (_frontend_notFoundRoute frontend)
  runWithHeadAndBody $ \appendHead appendBody -> runMyRouteViewT $ do
    mapRoutedT (mapEventWriterT appendHead) $ _frontend_head frontend
    mapRoutedT (mapEventWriterT appendBody) $ _frontend_body frontend

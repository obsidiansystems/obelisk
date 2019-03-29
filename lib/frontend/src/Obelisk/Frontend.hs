{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  , runFrontend
  , runFrontendWithCurrentRoute
  , renderFrontendHtml
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Sum
import Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text)
import GHCJS.DOM hiding (bracket, catch)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.History as DOM
import qualified GHCJS.DOM.Window as DOM
import Language.Javascript.JSaddle (JSM, jsNull)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import qualified Reflex.TriggerEvent.Base as TriggerEvent
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)

makePrisms ''Sum

type ObeliskWidget t x route m =
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
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadFix (Performable m)
  , PrimMonad m
  , Prerender x m
  , SetRoute t route m
  , RouteToUrl route m
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m x. ObeliskWidget t x route m => RoutedT t route m ())
  , _frontend_body :: !(forall t m x. ObeliskWidget t x route m => RoutedT t route m ())
  }

type Widget' x = ImmediateDomBuilderT DomTimeline (DomCoreWidget x)

-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget x = TriggerEventT DomTimeline (DomCoreWidget x)

type DomCoreWidget x = PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))

--TODO: Rename
{-# INLINABLE attachWidget''' #-}
attachWidget'''
  :: (EventChannel -> PerformEventT DomTimeline DomHost (IORef (Maybe (EventTrigger DomTimeline ()))))
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
        bodyElement <- getBodyUnchecked globalDoc
        void $ inAnimationFrame' $ \_ -> do
          replaceElementContents headElement headFragment
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

setInitialRoute :: JSM ()
setInitialRoute = do
  window <- DOM.currentWindowUnchecked
  initialLocation <- DOM.getLocation window
  initialUri <- getLocationUri initialLocation
  history <- DOM.getHistory window
  DOM.replaceState history jsNull ("" :: Text) $ Just $
    show $ setAdaptedUriPath "/" initialUri

-- | Run the frontend decoding the initial route from the browser's location URL.
runFrontendWithCurrentRoute :: forall backendRoute route. Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName -> Frontend (R route) -> JSM ()
runFrontendWithCurrentRoute validFullEncoder frontend = do
  let ve = validFullEncoder . hoistParse errorLeft (prismEncoder (rPrism $ _InR . _ObeliskRoute_App))
      errorLeft = \case
        Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
        Right x -> Identity x
      runMyRouteViewT
        :: ( TriggerEvent t m
           , PerformEvent t m
           , MonadHold t m
           , DOM.MonadJSM m
           , DOM.MonadJSM (Performable m)
           , MonadFix m
           , MonadFix (Performable m)
           )
        => RoutedT t (R route) (SetRouteT t (R route) (RouteToUrlT (R route) m)) a
        -> m a
      runMyRouteViewT = runRouteViewT ve
  runWithHeadAndBody $ \appendHead appendBody -> runMyRouteViewT $ do
    mapRoutedT (mapSetRouteT (mapRouteToUrlT appendHead)) $ _frontend_head frontend
    mapRoutedT (mapSetRouteT (mapRouteToUrlT appendBody)) $ _frontend_body frontend

-- | Run the frontend, setting the initial route to "/" on platforms where no
-- route exists ambiently in the context (e.g. anything but web).
runFrontend :: forall backendRoute route. Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName -> Frontend (R route) -> JSM ()
runFrontend validFullEncoder frontend = do
#ifndef ghcjs_HOST_OS
  setInitialRoute
#endif
  runFrontendWithCurrentRoute validFullEncoder frontend

renderFrontendHtml
  :: (t ~ DomTimeline)
  => (r' -> Text)
  -> r
  -> RoutedT t r (SetRouteT t r' (RouteToUrlT r' (PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))))) ()
  -> RoutedT t r (SetRouteT t r' (RouteToUrlT r' (PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))))) ()
  -> IO ByteString
renderFrontendHtml urlEnc route headWidget bodyWidget = do
  --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  html <- fmap snd $ renderStatic $ fmap fst $ flip runRouteToUrlT urlEnc $ runSetRouteT $ flip runRoutedT (pure route) $
    el "html" $ do
      el "head" $ do
        let baseTag = elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
        -- The order here is important - baseTag has to be before headWidget!
        baseTag >> injectExecutableConfigs >> headWidget
      el "body" bodyWidget
  return $ "<!DOCTYPE html>" <> html

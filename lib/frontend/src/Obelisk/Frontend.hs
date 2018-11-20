{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  , runFrontend
  , renderFrontendHtml
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Concurrent
import Control.Lens
import Control.Monad hiding (sequence, sequence_)
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Ref
import Control.Monad.Trans.Class
import Data.ByteString (ByteString)
import Data.Dependent.Sum (DSum (..))
import Data.Foldable (for_)
import Data.Functor.Sum
import Data.IORef
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Foldable (sequence_)
import Data.Traversable (sequence)
import GHCJS.DOM hiding (bracket, catch)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle (JSM)
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Dom.Builder.Hydration
import Reflex.Host.Class
import qualified Reflex.TriggerEvent.Base as TriggerEvent

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

type Widget' x a = HydrationDomBuilderT DomTimeline (DomCoreWidget x) a

-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget x = TriggerEventT DomTimeline (DomCoreWidget x)

type DomCoreWidget x = PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))

--TODO: Rename
{-# INLINABLE attachWidget''' #-}
attachWidget'''
  :: Document
  -> IORef HydrationMode
  -> IORef [(Node, [DOM DomTimeline (PostBuildT DomTimeline (WithJSContextSingleton () (PerformEventT DomTimeline DomHost)))])]
  -> JSContextSingleton ()
  -> (EventChannel -> PerformEventT DomTimeline DomHost (IORef (Maybe (EventTrigger DomTimeline ()))))
  -> IO ()
attachWidget''' doc hydrationMode hres jsSing w = do
  events <- newChan
  runDomHost $ flip runTriggerEventT events $ mdo
    (syncEvent, fireSync) <- newTriggerEvent
    liftIO $ putStrLn "-------------------------------------- Running host"
    (postBuildTriggerRef, fc@(FireCommand fire)) <- lift $ hostPerformEventT $ do
      a <- w events
      runWithReplace (return ()) $ delayedAction <$ syncEvent
      pure a
    mPostBuildTrigger <- readRef postBuildTriggerRef
    lift $ forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    liftIO $ fireSync ()
    forests <- liftIO $ readIORef hres
    -- TODO: DomHost is SpiderHost Global only if we're not in profiling mode
    let delayedAction = do
          for_ (reverse forests) $ \(rootNode, forest) -> do
            liftIO $ putStrLn "-------------------------------------- Performing delayed actions"
            void $ runWithJSContextSingleton (runPostBuildT (runHydrationRunnerT (runDOMForest forest) Nothing rootNode events) never) jsSing
            liftIO $ putStrLn "-------------------------------------- Performed delayed actions"
          liftIO $ writeIORef hydrationMode HydrationMode_Immediate
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
  headElement <- getHeadUnchecked globalDoc
  bodyElement <- getBodyUnchecked globalDoc
  unreadyChildren <- liftIO $ newIORef 0
  hydrationMode <- liftIO $ newIORef HydrationMode_Hydrating
  hydrationResult <- liftIO $ newIORef []
  liftIO $ attachWidget''' globalDoc hydrationMode hydrationResult jsSing $ \events -> flip runWithJSContextSingleton jsSing $ do
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let hydrateDom :: DOM.Node -> Widget' () c -> FloatingWidget () c
        hydrateDom n w = do
          events' <- TriggerEvent.askEvents
          parent <- liftIO $ newIORef $ toNode n
          prerenderDepth <- liftIO $ newIORef 0
          lift $ do
            let builderEnv = HydrationDomBuilderEnv
                  { _hydrationDomBuilderEnv_document = globalDoc
                  , _hydrationDomBuilderEnv_parent = parent
                  , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
                  , _hydrationDomBuilderEnv_commitAction = pure ()
                  , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
                  , _hydrationDomBuilderEnv_prerenderDepth = prerenderDepth
                  }
            (a, res) <- runHydrationDomBuilderT w builderEnv events'
            liftIO $ modifyIORef' hydrationResult ((n, res) :)
            pure a
    flip runPostBuildT postBuild $ flip runTriggerEventT events $ app (hydrateDom $ toNode headElement) (hydrateDom $ toNode bodyElement)
--    liftIO (readIORef unreadyChildren) >>= \case
--      0 -> DOM.liftJSM commit
--      _ -> return ()
    return postBuildTriggerRef

runFrontend :: forall backendRoute route. Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName -> Frontend (R route) -> JSM ()
runFrontend validFullEncoder frontend = do
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
      el "head" headWidget
      el "body" bodyWidget
  return $ "<!DOCTYPE html>" <> html

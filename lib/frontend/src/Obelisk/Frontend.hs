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
import Control.Monad.Reader
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
  , TriggerEvent t m
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadFix (Performable m)
  , PrimMonad m
  , Prerender t m
  , SetRoute t route m
  , SetRoute t route (Client m)
  , RouteToUrl route m
  , RouteToUrl route (Client m)
  , Monad (Client m)
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  , _frontend_body :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  }

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
        => Event t ()
        -> RoutedT t (R route) (SetRouteT t (R route) (RouteToUrlT (R route) m)) a
        -> m a
      runMyRouteViewT = runRouteViewT ve
  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> do
    rec switchover <- runMyRouteViewT switchover $ do
          (switchover, fire) <- newTriggerEvent
          mapRoutedT (mapSetRouteT (mapRouteToUrlT appendHead)) $ do
            _frontend_head frontend
          mapRoutedT (mapSetRouteT (mapRouteToUrlT appendBody)) $ do
            _frontend_body frontend
            switchover' <- lift $ lift $ lift $ HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
            performEvent_ $ liftIO (fire ()) <$ switchover'
          pure switchover
    pure ()

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

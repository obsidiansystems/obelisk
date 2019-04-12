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
  , module Obelisk.Frontend.Cookie
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Data.ByteString (ByteString)
import Data.Functor.Sum
import Data.Monoid ((<>))
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM)
import Obelisk.Frontend.Cookie
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)
import Web.Cookie

makePrisms ''Sum

type ObeliskWidget js t route m =
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
  , Prerender js t m
  , PrebuildAgnostic t route m
  , PrebuildAgnostic t route (Client m)
  , HasCookies m
  -- TODO Remove these. Probably requires a new class to allow executable-configs to work without being inside a `prerender`
  , MonadIO m
  , MonadIO (Performable m)
  )

type PrebuildAgnostic t route m =
  ( SetRoute t route m
  , RouteToUrl route m
  , MonadFix m
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall js t m. ObeliskWidget js t route m => RoutedT t route m ())
  , _frontend_body :: !(forall js t m. ObeliskWidget js t route m => RoutedT t route m ())
  }

runFrontend
  :: forall backendRoute route
  .  Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName
  -> Frontend (R route)
  -> JSM ()
runFrontend validFullEncoder frontend = do
  let ve = validFullEncoder . hoistParse errorLeft (prismEncoder (rPrism $ _InR . _ObeliskRoute_App))
      errorLeft = \case
        Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
        Right x -> Identity x
  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> do
    rec switchover <- runRouteViewT ve switchover $ do
          (switchover'', fire) <- newTriggerEvent
          mapRoutedT (mapSetRouteT (mapRouteToUrlT appendHead)) $ do
            _frontend_head frontend
          mapRoutedT (mapSetRouteT (mapRouteToUrlT appendBody)) $ do
            _frontend_body frontend
            switchover' <- lift $ lift $ lift $ HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
            performEvent_ $ liftIO (fire ()) <$ switchover'
          pure switchover''
    pure ()

renderFrontendHtml
  :: ( t ~ DomTimeline
     , MonadIO m
     , widget ~ RoutedT t r (SetRouteT t r' (RouteToUrlT r' (CookiesT (PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost))))))
     )
  => Cookies
  -> (r' -> Text)
  -> r
  -> widget ()
  -> widget ()
  -> m ByteString
renderFrontendHtml cookies urlEnc route headWidget bodyWidget = do
  --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  html <- fmap snd $ liftIO $ renderStatic $ fmap fst $ runCookiesT cookies $ flip runRouteToUrlT urlEnc $ runSetRouteT $ flip runRoutedT (pure route) $
    el "html" $ do
      el "head" $ do
        let baseTag = elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
        -- The order here is important - baseTag has to be before headWidget!
        baseTag >> injectExecutableConfigs >> headWidget
      el "body" bodyWidget
  return $ "<!DOCTYPE html>" <> html

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
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Functor.Sum
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text)
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (getHead)
import GHCJS.DOM.Element (getElementsByClassName)
import GHCJS.DOM.HTMLCollection (item, getLength)
import GHCJS.DOM.Node (removeChild_)
import Language.Javascript.JSaddle (JSM)
import Obelisk.Frontend.Config
import Obelisk.Frontend.Cookie
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)
import Obelisk.ExecutableConfig (getFrontendConfigs)
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
  , HasConfigs m
  , HasCookies m
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

baseTag :: forall route js t m. ObeliskWidget js t route m => RoutedT t route m ()
baseTag = elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes

removeHTMLConfigs :: JSM ()
removeHTMLConfigs = void $ runMaybeT $ do
  doc <- MaybeT currentDocument
  hd <- MaybeT $ getHead doc
  es <- collToList =<< getElementsByClassName hd ("obelisk-executable-config-inject" :: Text)
  for_ es $ removeChild_ hd
  where
    collToList es = do
      len <- getLength es
      lst <- traverse (item es) $ take (fromIntegral len) $ [0..] -- fun with unsigned types ...
      pure $ catMaybes lst

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
  configs <- liftIO getFrontendConfigs
  removeHTMLConfigs
  runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> do
    rec switchover <- runRouteViewT ve switchover $ do
          (switchover'', fire) <- newTriggerEvent
          mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendHead . runConfigsT configs))) $ do
            -- The order here is important - baseTag has to be before headWidget!
            baseTag
            _frontend_head frontend
          mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendBody . runConfigsT configs))) $ do
            _frontend_body frontend
            switchover' <- lift $ lift $ lift $ lift $ HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
            performEvent_ $ liftIO (fire ()) <$ switchover'
          pure switchover''
    pure ()

renderFrontendHtml
  :: ( t ~ DomTimeline
     , MonadIO m
     , widget ~ RoutedT t r (SetRouteT t r (RouteToUrlT r (ConfigsT (CookiesT (PostBuildT t (StaticDomBuilderT t (PerformEventT t DomHost)))))))
     )
  => Cookies
  -> (r -> Text)
  -> r
  -> Frontend r
  -> widget ()
  -> widget ()
  -> m ByteString
renderFrontendHtml cookies urlEnc route frontend headExtra bodyExtra = do
  --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  configs <- liftIO getFrontendConfigs
  html <- fmap snd $ liftIO $ renderStatic $ fmap fst $ runCookiesT cookies $ runConfigsT configs $ flip runRouteToUrlT urlEnc $ runSetRouteT $ flip runRoutedT (pure route) $
    el "html" $ do
      el "head" $ do
        baseTag
        injectExecutableConfigs
        _frontend_head frontend
        headExtra
      el "body" $ do
        _frontend_body frontend
        bodyExtra
  return $ "<!DOCTYPE html>" <> html

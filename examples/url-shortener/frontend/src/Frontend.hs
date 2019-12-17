{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Map as M
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Control.Monad.Fix (MonadFix)
import Language.Javascript.JSaddle (MonadJSM)

import Common.Api
import Common.Route
import Obelisk.Generated.Static

type AppWidget js t m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , TriggerEvent t m
  , PostBuild t m
  , PerformEvent t m
  , Prerender js t m
  )

type FrontendWidget js t m =
  ( AppWidget js t m
  , MonadJSM m
  , MonadJSM (Performable m)
  , HasJSContext (Performable m)
  )

data State = NotStarted | Loading | Loaded (Maybe Text)
  deriving (Eq)

submitAttrs :: State -> M.Map Text Text
submitAttrs = \case
  Loading -> ("disabled" =: "true")
  _ -> mempty

urlInput :: AppWidget js t m => m (Dynamic t State)
urlInput = do
  rec
    inputEl <- inputElement $ def
    (submitBtn,_) <- elAttr "span" ("id" =: "submit") $
      elDynAttr' "button" (submitAttrs <$> state) $ text "shorten"
    let click = domEvent Click submitBtn
    let url = tagPromptlyDyn (_inputElement_value inputEl) click
    request <- prerender
      (pure never)
      ((decodeXhrResponse <$>) <$> performRequestAsync (shortenRequest <$> url))
    state <- holdDyn NotStarted $
      leftmost [Loading <$ click, Loaded <$> switchDyn request]
    _ <- pure state
  pure state

createShortLinkRoute :: Text
createShortLinkRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_Shorten :/ ()

shortenRequest :: Text -> XhrRequest Text
shortenRequest s = postJson createShortLinkRoute (ShortenRequest s)

createdLink :: AppWidget js t m => State -> m ()
createdLink = \case
  NotStarted -> blank
  Loading -> text "shortening..."
  Loaded a -> case a of
    Nothing -> text "Error"
    Just url -> elAttr "a" ("href" =: url) $ text url

app :: AppWidget js t m => m ()
app = do
  state <- el "div" $ urlInput
  _ <- dyn $ createdLink <$> state
  blank

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Url Shortener"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"style.css") blank
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css") blank
  , _frontend_body = app
  }

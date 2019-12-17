{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

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

urlInput :: AppWidget js t m => m (Dynamic t State)
urlInput = do
  inputEl <- el "label" $ do
      text "url: "
      inputElement def
  click <- button "shorten"
  let url = tagPromptlyDyn (_inputElement_value inputEl) click
  request <- prerender
    (pure never)
    ((decodeXhrResponse <$>) <$> performRequestAsync (shortenRequest <$> url))
  let requestEv = switchDyn request
  holdDyn NotStarted $
    leftmost [Loading <$ click, Loaded <$> requestEv]

createShortLinkRoute :: Text
createShortLinkRoute = renderBackendRoute checkedFullRouteEncoder $ BackendRoute_Shorten :/ ()

shortenRequest :: Text -> XhrRequest Text
shortenRequest s = postJson createShortLinkRoute (ShortenRequest s)

createdLink :: AppWidget js t m => State -> m ()
createdLink = \case
  NotStarted -> blank
  Loading -> text "loading"
  Loaded a -> case a of
    Nothing -> text "Error"
    Just url -> elAttr "a" ("href" =: url) $ text url

app :: AppWidget js t m => m ()
app = do
  state <- urlInput
  _ <- dyn $ createdLink <$> state
  blank

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Url Shortener"
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: static @"style.css") blank
  , _frontend_body = app
  }

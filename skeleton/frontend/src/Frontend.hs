{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend where

import Control.Lens.Getter
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Reflex.Dom.Core
import Data.FileEmbed
import Language.Javascript.JSaddle (MonadJSM) 

import Common.Route
import Obelisk.Generated.Static

appRoute :: T.Text
appRoute =
  T.decodeUtf8 $(embedFile "config/common/route")
  & T.strip
  & T.dropWhileEnd (== '/')
  & T.breakOn "://"
  & snd

initialHelloText :: T.Text
initialHelloText = "The backend hasn't heard from you yet"

helloResponse
  :: ( DomBuilder t m
     , HasJSContext m     
     , MonadHold t m
     , MonadJSM m
     , MonadJSM (Performable m)
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     )
  => Event t ()
  -> m ()
helloResponse sendHello = do
  ws <- textWebSocket ("ws" <> appRoute <> "/hello") $ def
    & webSocketConfig_send .~ (["hi" :: T.Text] <$ sendHello)
  widgetHold_ (text initialHelloText) (text . ("The backend says '" <>) . (<> "'") <$> ws ^. webSocket_recv)

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "br" blank
      buttonClick <- button "Say hi to the backend"
      hasBeenClicked <- holdDyn False (True <$ buttonClick)
      _ <- dyn $ ffor hasBeenClicked $ \case
        False -> text initialHelloText
        True -> void $ prerender (text initialHelloText) $ helloResponse buttonClick
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s
      return ()
  }

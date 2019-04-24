{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import Control.Monad
import Data.Text
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      route <- askRoute
      initialRoute <- sample $ current route

      el "p" $ do
        text "Initial: "
        text $ pack $ show initialRoute

      el "p" $ do
        text "Current: "
        display route

      -- Remove this line and it works:
      void $ runWithReplace blank $ const blank <$> tag (current route) never

      el "p" $ do
        main <- button "Main"
        setRoute $ (FrontendRoute_Main :/ ()) <$ main

      el "p" $ do
        foo <- button "Foo"
        setRoute $ (FrontendRoute_Foo :/ ()) <$ foo
  }

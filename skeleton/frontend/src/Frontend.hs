{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Functor.Identity
import Data.Dependent.Sum
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Static

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = body
  }

body
  :: forall t m
  . ( PostBuild t m, DomBuilder t m
  , Routed t (R FrontendRoute) m, SetRoute t (R FrontendRoute) m
  ) => m ()
body = do
  text "Welcome to Obelisk!"
  el "p" $ text $ T.pack commonStuff
  elAttr "img" ("src" =: static @"obelisk.jpg") blank

  ti <- inputElement def
  el "p" $ display $ value ti
  e <- button "Switch route"

  r <- askRoute
  el "p" $ display r
  dyn_ $ ffor r $ \case
    FrontendRoute_Main :/ () -> setRoute $ (FrontendRoute_Other :/ []) <$ e
    FrontendRoute_Other :/ _rs -> setRoute $ (FrontendRoute_Main :/ ()) <$ e
    _ -> pure ()


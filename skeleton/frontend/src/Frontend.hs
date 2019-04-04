{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Frontend.Config
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      greeting <- getConfig "config/frontend/greeting"
      el "p" $ text $ fromMaybe "I forgot my line ... ~\\_(O_o)_/~" greeting
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad.IO.Class
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import qualified Obelisk.ExecutableConfig as Config
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static


frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      route <- askRoute
      toUrl <- askRouteToUrl
      Just host <- liftIO $ Config.get "config/common/route"
      el "div" $ dynText $ (host <>) . toUrl <$> route
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

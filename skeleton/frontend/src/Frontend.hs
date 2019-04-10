{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Obelisk.ExecutableConfig as EC
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
      greeting1 <- lift $ liftIO $ EC.get "config/frontend/greeting"
      el "p" $ text $ "old way: " <> fromMaybe "I forgot my line ... ~\\_(O_o)_/~" greeting1
      greeting2 <- getConfig "config/frontend/greeting"
      el "p" $ text $ "new way: " <> fromMaybe "I forgot my line ... ~\\_(O_o)_/~" greeting2
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank
  }

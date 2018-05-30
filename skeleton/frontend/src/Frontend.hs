{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Obelisk.ExecutableConfig.Types (Route)

import Common.Api
import Static

frontend :: Route -> (StaticWidget x (), Widget x ())
frontend route = (head', body)
  where
    head' = el "title" $ text "Obelisk Minimal Example"
    body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      el "p" $ do
        text $ "And here's our route: "
        el "tt" $ text $ T.pack $ show route
      elAttr "img" ("src" =: static @"obelisk.jpg") blank

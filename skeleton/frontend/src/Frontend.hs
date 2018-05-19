{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Reflex.Dom

import qualified Data.Text as T

import Common.Api
import Static

frontend :: (Widget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Obelisk Minimal Example"
    body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank

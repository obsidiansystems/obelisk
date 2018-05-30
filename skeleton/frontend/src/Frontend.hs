{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Static

staticHead, staticBody :: DomBuilder t m => m ()
staticHead = el "title" $ text "Obelisk Minimal Example"
staticBody = template $ el "p" $ text "JavaScript is required to view this page"

template :: DomBuilder t m => m () -> m ()
template = elAttr "div" ("style" =: "text-align: center")

frontend :: Widget x ()
frontend = template $ do
  text "Welcome to Obelisk!"
  el "p" $ text $ T.pack commonStuff
  elAttr "img" ("src" =: static @"obelisk.jpg") blank

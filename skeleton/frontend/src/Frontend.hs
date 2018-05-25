{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Obelisk.Frontend
import Static -- For cross platform embedding of static assets

data Page = Page_1 | Page_2 deriving Show

frontend :: ObeliskFrontend
frontend = ObeliskFrontend app

staticHead, staticBody :: DomBuilder t m => m ()
staticHead = el "title" $ text "Obelisk Minimal Example"
staticBody = template $ el "p" $ text "JavaScript is required to view this page"

template :: DomBuilder t m => m () -> m ()
template = elAttr "div" ("style" =: "text-align: center")

app :: MonadWidget t m => m ()
app = template $ do
  el "h1" $ text "Welcome to Obelisk!"
  el "p" $ text $ T.pack commonStuff
  elAttr "img" ("src" =: static @"obelisk.jpg") blank

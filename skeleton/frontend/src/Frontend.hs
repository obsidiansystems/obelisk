{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Frontend where

import Reflex.Dom

import qualified Data.Text as T

import Static
import Common.Api

frontend :: Widget x ()
frontend = do
  text "Welcome to Obelisk!"
  el "p" $ text $ T.pack commonStuff
  elAttr "img" ("src" =: static @"obelisk.jpg") blank


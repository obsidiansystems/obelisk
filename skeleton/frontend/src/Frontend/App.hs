{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Frontend.App where

import Reflex.Dom
import Static

frontend :: IO ()
frontend = mainWidget $ do
  text "Welcome to Obelisk!"
  elAttr "img" ("src" =: static @"obelisk.jpg") blank

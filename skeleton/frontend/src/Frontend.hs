{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Frontend where

import Reflex.Dom
import Static

frontend :: Widget x ()
frontend = do
  text "Welcome to Obelisk!"
  elAttr "img" ("src" =: static @"obelisk.jpg") blank


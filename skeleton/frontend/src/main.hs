{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
import Reflex.Dom
import Static

main :: IO ()
main = mainWidget $ do
  text "Welcome to Obelisk!"
  elAttr "img" ("src" =: static @"obelisk.jpg") blank

{-# LANGUAGE ScopedTypeVariables #-}
module Devel where

import Obelisk.Run

import Backend
import Frontend

import Obelisk.ExecutableConfig (get)
import Obelisk.ExecutableConfig.Types (Route)

main :: Int -> IO ()
main port = do
  -- XXX: uhh, do DRY
  route :: Route <- get
  run port backend $ frontend route

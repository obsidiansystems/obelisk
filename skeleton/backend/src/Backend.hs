{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Common.Api
import Frontend
import qualified Obelisk.Backend as Ob

import Obelisk.ExecutableConfig (get)
import Obelisk.ExecutableConfig.Types (Route)

backend :: IO ()
backend = do
  route :: Route <- get
  Ob.backend Ob.def
    { Ob._backendConfig_head = fst $ frontend route
    }

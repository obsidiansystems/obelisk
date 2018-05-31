{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Backend where

import Common.Api
import Frontend
import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = do
  Ob.backend Ob.def
    { Ob._backendConfig_head = fst frontend
    }

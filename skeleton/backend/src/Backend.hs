{-# LANGUAGE OverloadedStrings #-}
module Backend where

import qualified Data.ByteString.Char8 as BSC

import qualified Obelisk.Backend as Ob
import Snap (Snap, writeBS)

import Common.Api (commonStuff)
import Frontend (frontend)

backend :: IO ()
backend = Ob.backend $ Ob.def
  { Ob._backendConfig_head = fst frontend
  , Ob._backendConfig_routes =
      [ ("/api", someApi)
      ]
  }

someApi :: Snap ()
someApi = do
  writeBS $ BSC.pack commonStuff

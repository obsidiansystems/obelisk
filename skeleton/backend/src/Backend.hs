{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend where

import Obelisk.Backend
import System.Process
import System.Which

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> do
      _ <- spawnProcess $(staticWhich "git") ["clone", "git@github.com:obsidiansystems/obelisk.git"]
      pure ()

  , _backend_routeEncoder = fullRouteEncoder
  }

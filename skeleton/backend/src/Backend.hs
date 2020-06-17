{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Backend where

import Control.Monad
import Obelisk.App
import Obelisk.CliApp
import Obelisk.Command
import Obelisk.Backend
import System.Which

import Common.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \_serve -> do
      cfg <- mkObeliskConfig
      print <=< runObelisk cfg
        $ readProcessAndLogOutput (Debug, Debug)
        $ proc $(staticWhich "git") ["clone", "git@github.com:obsidiansystems/obelisk.git"]

  , _backend_routeEncoder = fullRouteEncoder
  }

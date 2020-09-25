{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Backend where

import Control.Monad.IO.Class
import Common.Route
import Obelisk.Backend
import Frontend

backend :: Backend DomainRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
    DomainRoute_A -> \r -> liftIO $ print r
    DomainRoute_B -> \r -> liftIO $ print r
  , _backend_routeEncoder = myMkFullRouteEncoder
  , _backend_obRunBaseRoute = baseRoute
  , _backend_frontend = \case
    DomainRoute_A -> frontendA
    DomainRoute_B -> frontendB
  , _backend_frontendName = \case
    DomainRoute_A -> "frontend-a"
    DomainRoute_B -> "frontend-b"
  }

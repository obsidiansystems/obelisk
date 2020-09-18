module Backend where

import Common.Route
import Obelisk.Route
import Obelisk.Backend

backend :: Backend () (AppRoute BackendRoute) (AppRoute FrontendRoute)
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

module Backend (backend, frontend) where

import Common.Route
import Frontend (frontend)
import Obelisk.Backend

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

module Backend where

import Common.Route
import Common.Route.Checked
import Data.Functor.Identity
import Obelisk.Backend
import Obelisk.Route

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = hoistCheck (pure . runIdentity) validFullEncoder
  }

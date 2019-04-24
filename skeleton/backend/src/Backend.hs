{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Snap

backend :: Backend BackendRoute FrontendRoute Snap AppConfig
backend = Backend
  { _backend_routeEncoder = backendRouteEncoder
  , _backend_routes = const $ return ()
  , _backend_runner = httpServe
  }

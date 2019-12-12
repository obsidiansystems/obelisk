{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      serve $ \case 
        BackendRoute_Missing :/ () -> do
          modifyResponse $ setResponseStatus 400 "Malformed Request"
          writeBS "400 Malformed Request"
          r <- getResponse
          finishWith r
      return ()
  , _backend_routeEncoder = backendRouteEncoder
  }

{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Prelude hiding (id)
import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core
import Gargoyle.PostgreSQL.Connect

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool ->
        serve $ \case
          BackendRoute_Missing :/ () -> do
            modifyResponse $ setResponseStatus 400 "Malformed Request"
            writeBS "400 Malformed Request"
            r <- getResponse
            finishWith r
          BackendRoute_Shorten :/ () -> do
            modifyResponse $ setResponseStatus 200 "OK"
            writeBS $ "\"OK\""
            r <- getResponse
            finishWith r
          _ -> error "no"
      return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

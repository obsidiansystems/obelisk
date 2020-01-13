{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Route
import qualified Data.Aeson as A
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import qualified Network.WebSockets      as WS
import qualified Network.WebSockets.Snap as WS
import Obelisk.Route (pattern (:/))
import Obelisk.Backend
import qualified Snap.Core as S

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve ->
      serve $ \case

        BackendRoute_Hello :/ () -> do
          S.modifyResponse $ S.setResponseStatus 200 "OK"
          S.writeBS $ toStrict $ A.encode ("Hello!" :: T.Text)
          WS.runWebSocketsSnap $ \pending -> do
            conn <- WS.acceptRequest pending
            _ <- WS.receive conn
            WS.sendTextData conn ("Hello!" :: T.Text)

        _ -> pure ()
  , _backend_routeEncoder = fullRouteEncoder
  }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}

module Frontend where

import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle (eval, liftJSM)
import Prelude hiding (log)
import Obelisk.Frontend
import Obelisk.Configs
import Obelisk.Route
import Obelisk.Generated.Static

import Reflex.Dom.Core

import Common.Api
import Common.Route


-- This runs in a monad that can be run on the client or the server.
-- To run code in a pure client or pure server context, use one of the
-- `prerender` functions.
frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
      el "title" $ text "Obelisk Minimal Example"
      elAttr "link" ("href" =: static @"main.css" <> "type" =: "text/css" <> "rel" =: "stylesheet") blank
  , _frontend_body = do
      el "h1" $ text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff

      -- `prerender` and `prerender_` let you choose a widget to run on the server
      -- during prerendering and a different widget to run on the client with
      -- JavaScript. The following will generate a `blank` widget on the server and
      -- print "Hello, World!" on the client.
      prerender_ blank $ liftJSM $ void $ eval ("console.log('Hello, World!')" :: T.Text)

      elAttr "img" ("src" =: static @"obelisk.jpg") blank
      el "div" $ do
        exampleConfig <- getConfig "common/example"
        case exampleConfig of
          Nothing -> text "No config file found in config/common/example"
          Just s -> text $ T.decodeUtf8 s

      let url = "http://localhost:65536"
          req = xhrRequest "GET" url def
          cb = log . \case
            Left e -> "Left: " <> tshow e
            Right r -> ("Right: " <>) $ T.intercalate " / "
              [ "status: " <> tshow (_xhrResponse_status r)
              , "response: " <> tshow (_xhrResponse_responseText r)
              , "headers: " <> tshow (_xhrResponse_headers r)
              ]
          log txt = liftJSM $ void $ eval ("console.log('" <> txt <> "')" :: T.Text)
          tshow = T.pack . show

      prerender_ blank $ do
        log "0"
        void $ newXMLHttpRequestWithError req cb
        log "1"
        pb <- getPostBuild
        log "2"
        res <- getAndDecode (url <$ pb)
        log "3"
        void $ runWithReplace blank $ ffor res $ log . \case
          Nothing -> "Got nothing"
          Just () -> "Got something"
        log "4"

      return ()
  }

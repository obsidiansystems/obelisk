{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Control.Monad.IO.Class
import Data.Word
import Data.ByteString.Lazy
import Database.PostgreSQL.Simple
import Database.Id.Class
import Data.Text (Text)
import qualified Data.Aeson as A
import Data.Pool
import Prelude hiding (id)
import Common.Route
import Obelisk.Backend
import Obelisk.Route
import Snap.Core
import Gargoyle.PostgreSQL.Connect

maxUrlSize :: Word64
maxUrlSize = 2000

migration :: Query
migration = "CREATE TABLE IF NOT EXISTS urls\
  \ (id SERIAL PRIMARY KEY, url VARCHAR(?) NOT NULL);"

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDb "db" $ \pool -> do
        _ <- withResource pool $ \dbcon -> execute dbcon migration [maxUrlSize]
        serve $ \case

          BackendRoute_Get_Url :/ id -> do
            result <- liftIO $ withResource pool $ \dbcon ->
              query dbcon "SELECT (url) FROM urls WHERE id = ?;" [unId id]
            case result of
              [[url]] -> redirect url
              _ -> do
                modifyResponse $ setResponseStatus 404 "Not Found"
                writeText "That shortlink wasn't found ¯\\_(ツ)_/¯"
                r <- getResponse
                finishWith r

          BackendRoute_Shorten :/ () -> do
            Just url <- A.decode <$> readRequestBody maxUrlSize
            [[id]] <- liftIO $ withResource pool $ \dbcon -> do
                _ <- execute dbcon "INSERT INTO urls (url) values (?);" [url :: Text]
                query_ dbcon "SELECT last_value FROM urls_id_seq;"
            modifyResponse $ setResponseStatus 200 "OK"
            writeBS $ toStrict $ A.encode $ ("/s/" <>) $ show (id :: Int)
            r <- getResponse
            finishWith r

          _ -> do
            modifyResponse $ setResponseStatus 400 "Malformed Request"
            writeBS "400 Malformed Request"
            r <- getResponse
            finishWith r
      return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

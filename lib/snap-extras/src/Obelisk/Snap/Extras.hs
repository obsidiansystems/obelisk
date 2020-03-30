{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Snap.Extras
  ( cachePermanently
  , doNotCache
  , ensureSecure
  , serveFileIfExists
  , serveFileIfExistsAs
  ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.String
import Snap.Core
import Snap.Util.FileServe
import System.Directory

-- | Set response header for "permanent" caching
cachePermanently :: MonadSnap m => m ()
cachePermanently = do
  modifyResponse $ setHeader "Cache-Control" "public, max-age=315360000"
  modifyResponse $ setHeader "Expires" "Tue, 01 Feb 2050 00:00:00 GMT" --TODO: This should be set to "approximately one year from the time the response is sent"

-- | Set response header to not cache
doNotCache :: MonadSnap m => m ()
doNotCache = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  modifyResponse $ setHeader "Expires" "0"

-- | Serves the specified file if it exists; otherwise, 'pass'es
serveFileIfExists :: MonadSnap m => FilePath -> m ()
serveFileIfExists f = do
  exists <- liftIO $ doesFileExist f
  if exists then serveFile f else pass

-- | Like 'serveFileIfExists', but with a given MIME type
serveFileIfExistsAs :: MonadSnap m => ByteString -> FilePath -> m ()
serveFileIfExistsAs mimeType f = do
  exists <- liftIO $ doesFileExist f
  if exists then serveFileAs mimeType f else pass

-- | Only run the given handler when the connection is "secure" (i.e. made with HTTPS)
ensureSecure
  :: MonadSnap m
  => Int -- ^ The port where this server answers HTTPS requests
  -> m () -- ^ A handler to be run only when the connection is secure
  -> m ()
ensureSecure port h = do
  s <- getsRequest rqIsSecure
  if s then h else do
    uri <- getsRequest rqURI
    host <- getsRequest rqHostName --TODO: It might be better to use the canonical base of the server
    redirect $ "https://" <> host <> (if port == 443 then "" else ":" <> fromString (show port)) <> uri

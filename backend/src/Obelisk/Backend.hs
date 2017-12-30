{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Backend
  ( backend
  , BackendConfig
  -- * Re-exports
  , Default (def)
  ) where

import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Obelisk.Asset.Serve (serveAssets)
import System.IO (hSetBuffering, stderr, BufferMode (..))
import Snap (httpServe, defaultConfig, commandLineConfig, route)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig = BackendConfig

instance Default BackendConfig where
  def = BackendConfig

-- | Start an Obelisk backend
backend :: BackendConfig -> IO ()
backend _ = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  httpServe httpConf $ route
    [ ("", serveAssets "frontend.jsexe.assets" "frontend.jsexe")
    , ("", serveAssets "static.assets" "static")
    ]

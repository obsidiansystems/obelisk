{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Backend
  ( backend
  , BackendConfig (..)
  -- * Re-exports
  , Default (def)
  ) where

import Control.Lens
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Semigroup ((<>))
import System.IO (BufferMode (..), hSetBuffering, stderr)

import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.Snap
import Reflex.Dom
import Snap (Snap, commandLineConfig, defaultConfig, httpServe, route)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig = BackendConfig
  { _backendConfig_head :: StaticWidget () ()
  , _backendConfig_routes :: [(BS.ByteString, Snap ())]
  }

instance Default BackendConfig where
  def = BackendConfig (pure ()) def

-- | Start an Obelisk backend
backend :: BackendConfig -> IO ()
backend cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  headHtml <- fmap snd $ renderStatic $ _backendConfig_head cfg
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
      appCfg = def & appConfig_initialHead .~ headHtml
      routes =
        [ ("", serveApp "" appCfg)
        , ("", serveAssets "frontend.jsexe.assets" "frontend.jsexe") --TODO: Can we prevent naming conflicts between frontend.jsexe and static?
        , ("", serveAssets "static.assets" "static")
        ] <> _backendConfig_routes cfg
  -- Start the web server
  httpServe httpConf $ route routes

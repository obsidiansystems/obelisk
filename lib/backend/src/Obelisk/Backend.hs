{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Backend
  ( backend
  , BackendConfig (..)
  , StaticContent (..)
  , ReflexStaticContent
  -- * Re-exports
  , Default (def)
  ) where

import Control.Lens
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Obelisk.Asset.Serve.Snap (serveAssets)
import Obelisk.Snap
import Reflex.Dom
import System.IO (hSetBuffering, stderr, BufferMode (..))
import Snap (httpServe, defaultConfig, commandLineConfig, route)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig = BackendConfig
  { _backendConfig_serveJsexe :: Bool
  , _backendConfig_jsexe :: FilePath
  }

instance Default BackendConfig where
  def = BackendConfig
    { _backendConfig_serveJsexe = True
    , _backendConfig_jsexe = "frontend.jsexe"
    }

-- | Start an Obelisk backend
backend :: ReflexStaticContent -> BackendConfig -> IO ()
backend static cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  rendered <- renderStaticContent static
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
      appCfg = def
        & appConfig_initialHead .~ _staticContent_head rendered
        & appConfig_initialBody .~ _staticContent_body rendered
        & appConfig_serveJsexe .~ _backendConfig_serveJsexe cfg
        & appConfig_jsexe .~ _backendConfig_jsexe cfg
  -- Start the web server
  httpServe httpConf $ route
    [ ("", serveApp "" static appCfg)
    , ("", serveAssets "frontend.jsexe.assets" "frontend.jsexe") --TODO: Can we prevent naming conflicts between frontend.jsexe and static?
    , ("", serveAssets "static.assets" "static")
    ]

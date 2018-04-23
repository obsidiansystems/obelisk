{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Widget.Run where

import Data.ByteString (ByteString)
import Language.Javascript.JSaddle.WebSockets
import Language.Javascript.JSaddle.Run (syncPoint)
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import qualified Network.HTTP.ReverseProxy as RP
import Network.Wai (Application)
import Network.Wai.Handler.Warp (defaultSettings, setTimeout, setPort, runSettings)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Reflex.Dom.Core

runWidget :: RunConfig -> Widget () () -> IO ()
runWidget conf w = do
  man <- newManager defaultManagerSettings
  let redirectHost = _runConfig_redirectHost conf
      redirectPort = _runConfig_redirectPort conf
  runSettings (setPort (_runConfig_port conf) (setTimeout 3600 defaultSettings)) =<<
    jsaddleWithAppOr defaultConnectionOptions (mainWidget' w >> syncPoint) (fallbackProxy redirectHost redirectPort man)

fallbackProxy :: ByteString -> Int -> Manager -> Application
fallbackProxy host port = RP.waiProxyTo handleRequest RP.defaultOnExc
  where handleRequest _req = return $ RP.WPRProxyDest $ RP.ProxyDest host port

data RunConfig = RunConfig
  { _runConfig_port :: Int
  , _runConfig_redirectHost :: ByteString
  , _runConfig_redirectPort :: Int
  }

defRunConfig :: RunConfig
defRunConfig = RunConfig
  { _runConfig_port = 3000
  , _runConfig_redirectHost = "0.0.0.0"
  , _runConfig_redirectPort = 3001
  }

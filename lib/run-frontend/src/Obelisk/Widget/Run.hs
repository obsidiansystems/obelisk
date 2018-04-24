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

test :: IO ()
test = do
  runWidget $ do
    text "hello world"
    elAttr "img" ("src" =: "/obelisk.jpg") $ return ()

runWidget :: Widget () () -> IO ()
runWidget w = do
  man <- newManager defaultManagerSettings
  let port = 3003 -- TODO
      redirectHost = "0.0.0.0"
      redirectPort = 38383
  runSettings (setPort port (setTimeout 3600 defaultSettings)) =<<
    jsaddleWithAppOr defaultConnectionOptions (mainWidget' w >> syncPoint) (fallbackProxy redirectHost redirectPort man)

fallbackProxy :: ByteString -> Int -> Manager -> Application
fallbackProxy host port = RP.waiProxyTo handleRequest RP.defaultOnExc
  where handleRequest _req = return $ RP.WPRProxyDest $ RP.ProxyDest host port


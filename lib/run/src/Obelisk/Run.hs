{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Run where

import Control.Concurrent
import Control.Exception
import Control.Lens ((^?), _Just, _Right)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.List (uncons)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Streaming.Network (bindPortTCP)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.HTTP.Types as H
import Network.Socket
import Network.Wai (Application)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (settingsHost, settingsPort)
import Network.WebSockets (ConnectionOptions)
import Network.WebSockets.Connection (defaultConnectionOptions)
import Obelisk.ExecutableConfig (get)
import Reflex.Dom.Core
import System.Environment
import System.IO
import System.Process
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens

run :: Int -- ^ Port to run the backend
    -> IO () -- ^ Backend
    -> (StaticWidget () (), Widget () ()) -- ^ Frontend widget (head, body)
    -> IO ()
run port backend frontend = do
  let handleBackendErr (_ :: IOException) = hPutStrLn stderr "backend stopped; make a change to your code to reload"
  backendTid <- forkIO $ handle handleBackendErr $ withArgs ["--quiet", "--port", show port] backend
  putStrLn $ "Backend running on port " <> show port
  let conf = defRunConfig { _runConfig_redirectPort = port }
  runWidget conf frontend `finally` killThread backendTid

getConfigRoute :: IO (Maybe URI)
getConfigRoute = get "common/route" >>= \case
  Just r -> case URI.mkURI $ fromMaybe r $ T.stripSuffix "\n" r of
    Just route -> pure $ Just route
    Nothing -> do
      putStrLn $ "Route is invalid: " <> show r
      pure Nothing
  Nothing -> pure Nothing

defAppUri :: URI
defAppUri = fromMaybe (error "defAppUri") $ URI.mkURI "http://127.0.0.1:8000"

runWidget :: RunConfig -> (StaticWidget () (), Widget () ()) -> IO ()
runWidget conf (h, b) = do
  uri <- fromMaybe defAppUri <$> getConfigRoute
  let port = fromIntegral $ fromMaybe 80 $ uri ^? uriAuthority . _Right . authPort . _Just
      redirectHost = _runConfig_redirectHost conf
      redirectPort = _runConfig_redirectPort conf
      beforeMainLoop = do
        putStrLn $ "Frontend running on " <> T.unpack (URI.render uri)
      settings = setBeforeMainLoop beforeMainLoop (setPort port (setTimeout 3600 defaultSettings))
  bracket
    (bindPortTCPRetry settings (logPortBindErr port) (_runConfig_retryTimeout conf))
    close
    (\skt -> do
        man <- newManager defaultManagerSettings
        app <- obeliskApp defaultConnectionOptions h b (fallbackProxy redirectHost redirectPort man)
        runSettingsSocket settings skt app)

obeliskApp :: ConnectionOptions -> StaticWidget () () -> Widget () () -> Application -> IO Application
obeliskApp opts h b backend = do
  html <- BSLC.fromStrict <$> indexHtml h
  let entryPoint = mainWidget' b >> syncPoint
  jsaddleOr opts entryPoint $ \req sendResponse -> case (W.requestMethod req, W.pathInfo req) of
    ("GET", []) -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] html
    ("GET", ["jsaddle.js"]) -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] $ jsaddleJs False
    _ -> backend req sendResponse

indexHtml :: StaticWidget () () -> IO ByteString
indexHtml h = do
  ((), bs) <- renderStatic $ el "html" $ do
    el "head" $ h
    el "body" $ return ()
    elAttr "script" ("src" =: "/jsaddle.js") $ return ()
  return $ "<!DOCTYPE html>" <> bs

-- | like 'bindPortTCP' but reconnects on exception
bindPortTCPRetry :: Settings
                 -> (IOError -> IO ()) -- ^ Action to run the first time an exception is caught
                 -> Int
                 -> IO Socket
bindPortTCPRetry settings m n = catch (bindPortTCP (settingsPort settings) (settingsHost settings)) $ \(e :: IOError) -> do
  m e
  threadDelay $ 1000000 * n
  bindPortTCPRetry settings (\_ -> pure ()) n

logPortBindErr :: Int -> IOError -> IO ()
logPortBindErr p e = getProcessIdForPort p >>= \case
  Nothing -> putStrLn $ "runWidget: " <> show e
  Just pid -> putStrLn $ unwords [ "Port", show p, "is being used by process ID", show pid <> ".", "Please kill that process or change the port in config/common/route."]

getProcessIdForPort :: Int -> IO (Maybe Int)
getProcessIdForPort port = do
  xs <- lines <$> readProcess "ss" ["-lptn", "sport = " <> show port] mempty
  case uncons xs of
    Just (_, x:_) -> return $ A.maybeResult $ A.parse parseSsPid $ BSC.pack x
    _ -> return Nothing

parseSsPid :: A.Parser Int
parseSsPid = do
  _ <- A.count 5 $ A.takeWhile (not . A.isSpace) *> A.skipSpace
  _ <- A.skipWhile (/= ':') >> A.string ":((" >> A.skipWhile (/= ',')
  A.string ",pid=" *> A.decimal

fallbackProxy :: ByteString -> Int -> Manager -> Application
fallbackProxy host port = RP.waiProxyTo handleRequest RP.defaultOnExc
  where handleRequest _req = return $ RP.WPRProxyDest $ RP.ProxyDest host port

data RunConfig = RunConfig
  { _runConfig_port :: Int
  , _runConfig_redirectHost :: ByteString
  , _runConfig_redirectPort :: Int
  , _runConfig_retryTimeout :: Int -- seconds
  }

defRunConfig :: RunConfig
defRunConfig = RunConfig
  { _runConfig_port = 8000
  , _runConfig_redirectHost = "127.0.0.1"
  , _runConfig_redirectPort = 3001
  , _runConfig_retryTimeout = 1
  }

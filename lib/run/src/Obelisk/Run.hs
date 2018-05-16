{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Run where

import Control.Concurrent
import Control.Exception
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.List (uncons)
import Data.Semigroup ((<>))
import Data.Streaming.Network (bindPortTCP)
import Language.Javascript.JSaddle.WebSockets
import Language.Javascript.JSaddle.Run (syncPoint)
import Network.HTTP.Client (defaultManagerSettings, newManager, Manager)
import qualified Network.HTTP.ReverseProxy as RP
import Network.Socket
import Network.URI
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.WebSockets.Connection (defaultConnectionOptions)
import Network.Wai.Handler.Warp.Internal (settingsPort, settingsHost)
import Reflex.Dom.Core
import System.Environment
import System.IO
import System.Process

run :: Int -- ^ Port to run the backend
    -> IO () -- ^ Backend
    -> Widget () () -- ^ Frontend widget
    -> IO ()
run port backend frontend = do
  let handleBackendErr (_ :: SomeException) = hPutStrLn stderr "backend stopped; make a change to your code to reload"
  backendTid <- forkIO $ handle handleBackendErr $ withArgs ["--quiet", "--port", show port] backend
  let conf = defRunConfig { _runConfig_redirectPort = port }
  runWidget conf frontend
  killThread backendTid

runWidget :: RunConfig -> Widget () () -> IO ()
runWidget conf w = do
  let redirectHost = _runConfig_redirectHost conf
      redirectPort = _runConfig_redirectPort conf
      beforeMainLoop = do
        putStrLn $ "Frontend running on " <> showUrl "127.0.0.1" (_runConfig_port conf)
      settings = setBeforeMainLoop beforeMainLoop (setPort (_runConfig_port conf) (setTimeout 3600 defaultSettings))
      logErr p = putStrLn $ unwords [ "Port", show p, "is in use."]
  bracket
    (bindPortTCPRetry settings (logErr (_runConfig_port conf)) (_runConfig_retryTimeout conf))
    close
    (\skt -> do
        man <- newManager defaultManagerSettings
        app <- jsaddleWithAppOr defaultConnectionOptions (mainWidget' w >> syncPoint) (fallbackProxy redirectHost redirectPort man)
        runSettingsSocket settings skt app)

-- | like 'bindPortTCP' but reconnects on exception
bindPortTCPRetry :: Settings
                 -> IO () -- ^ Action to run the first time an exception is caught
                 -> Int
                 -> IO Socket
bindPortTCPRetry settings m n = catch (bindPortTCP (settingsPort settings) (settingsHost settings)) $ \(_ :: IOError) -> do
  m
  threadDelay $ 1000000 * n
  bindPortTCPRetry settings (return ()) n

-- TODO
logPortBindErr :: Int -> IO ()
logPortBindErr p = getProcessIdForPort p >>= \case
  Nothing -> return ()
  Just _ -> return ()

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

showUrl :: String -> Int -> String
showUrl host port = show nullURI
  { uriScheme = "http:"
  , uriAuthority = Just $ URIAuth "" host $ ":" ++ show port
  }

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

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Due to instance HasJS x (EventWriterT t w m)
module Obelisk.Run where

import Prelude hiding ((.), id)

import Control.Category
import Control.Concurrent
import Control.Exception
import Control.Lens ((%~), (^?), _Just, _Right)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.Functor.Sum
import Data.List (uncons)
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Streaming.Network (bindPortTCP)
import Data.Text (Text)
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
import qualified Obelisk.Asset.Serve.Snap as Snap
import Obelisk.ExecutableConfig (get)
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Snap.Core (Snap)
import System.Environment
import System.IO
import System.Process
import System.Exit (ExitCode(..))
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens
import Obelisk.Backend

run
  :: Int -- ^ Port to run the backend
  -> ([Text] -> Snap ()) -- ^ Static asset handler
  -> Backend fullRoute frontendRoute -- ^ Backend
  -> Frontend (R frontendRoute) -- ^ Frontend
  -> IO ()
run port serveStaticAsset backend frontend = do
  prettifyOutput
  let handleBackendErr (e :: IOException) = hPutStrLn stderr $ "backend stopped; make a change to your code to reload - error " <> show e
  --TODO: Use Obelisk.Backend.runBackend; this will require separating the checking and running phases
  case checkEncoder $ _backend_routeEncoder backend of
    Left e -> hPutStrLn stderr $ "backend error:\n" <> T.unpack e
    Right validFullEncoder -> do
      backendTid <- forkIO $ handle handleBackendErr $ withArgs ["--quiet", "--port", show port] $ do
        _backend_run backend $ \serveRoute -> do
          runSnapWithCommandLineArgs $ do
            getRouteWith validFullEncoder >>= \case
              Identity r -> case r of
                InL backendRoute :=> Identity a -> serveRoute $ backendRoute :/ a
                InR obeliskRoute :=> Identity a ->
                  serveDefaultObeliskApp (mkRouteToUrl validFullEncoder) serveStaticAsset frontend $ obeliskRoute :/ a
      let conf = defRunConfig { _runConfig_redirectPort = port }
      runWidget conf frontend validFullEncoder `finally` killThread backendTid

-- Convenience wrapper to handle path segments for 'Snap.serveAsset'
runServeAsset :: FilePath -> [Text] -> Snap ()
runServeAsset rootPath = Snap.serveAsset "" rootPath . T.unpack . T.intercalate "/"

getConfigRoute :: IO (Maybe URI)
getConfigRoute = get "config/common/route" >>= \case
  Just r -> case URI.mkURI $ T.strip r of
    Just route -> pure $ Just route
    Nothing -> do
      putStrLn $ "Route is invalid: " <> show r
      pure Nothing
  Nothing -> pure Nothing

defAppUri :: URI
defAppUri = fromMaybe (error "defAppUri") $ URI.mkURI "http://127.0.0.1:8000"

runWidget :: RunConfig -> Frontend (R route) -> Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName -> IO ()
runWidget conf frontend validFullEncoder = do
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
        app <- obeliskApp defaultConnectionOptions frontend validFullEncoder uri $ fallbackProxy redirectHost redirectPort man
        runSettingsSocket settings skt app)

obeliskApp
  :: forall route backendRoute. ConnectionOptions
  -> Frontend (R route)
  -> Encoder Identity Identity (R (Sum backendRoute (ObeliskRoute route))) PageName
  -> URI
  -> Application
  -> IO Application
obeliskApp opts frontend validFullEncoder uri backend = do
  let entryPoint = do
        runFrontend validFullEncoder frontend
        syncPoint
  jsaddlePath <- URI.mkPathPiece "jsaddle"
  let jsaddleUri = BSLC.fromStrict $ URI.renderBs $ uri & uriPath %~ (<>[jsaddlePath])
  Right (jsaddleWarpRouteValidEncoder :: Encoder Identity (Either Text) (R JSaddleWarpRoute) PageName) <- return $ checkEncoder jsaddleWarpRouteEncoder
  jsaddle <- jsaddleWithAppOr opts entryPoint $ \_ sendResponse -> sendResponse $ W.responseLBS H.status500 [("Content-Type", "text/plain")] "obeliskApp: jsaddle got a bad URL"
  return $ \req sendResponse -> case tryDecode validFullEncoder (W.pathInfo req, mempty) of --TODO: Query strings
    Identity r -> case r of
      InR (ObeliskRoute_Resource ResourceRoute_JSaddleWarp) :=> Identity jsaddleRoute -> case jsaddleRoute of
        JSaddleWarpRoute_JavaScript :/ () -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] $ jsaddleJs' (Just jsaddleUri) False
        _ -> flip jsaddle sendResponse $ req
          { W.pathInfo = fst $ encode jsaddleWarpRouteValidEncoder jsaddleRoute
          }
      InR (ObeliskRoute_App appRouteComponent) :=> Identity appRouteRest -> do
        html <- renderJsaddleFrontend (mkRouteToUrl validFullEncoder) (appRouteComponent :/ appRouteRest) frontend
        sendResponse $ W.responseLBS H.status200 [("Content-Type", staticRenderContentType)] $ BSLC.fromStrict html
      _ -> backend req sendResponse

renderJsaddleFrontend :: (route -> Text) -> route -> Frontend route -> IO ByteString
renderJsaddleFrontend urlEnc r f =
  let jsaddleScript = elAttr "script" ("src" =: "/jsaddle/jsaddle.js") blank
      jsaddlePreload = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: "/jsaddle/jsaddle.js") blank
  in renderFrontendHtml urlEnc r (_frontend_head f >> jsaddlePreload) (_frontend_body f >> jsaddleScript)

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
  -- First check if 'ss' is available
  (c, _, _) <- readProcessWithExitCode "which" ["ss"] mempty
  case c of
   ExitSuccess -> do
     xs <- lines <$> readProcess "ss" ["-lptn", "sport = " <> show port] mempty
     case uncons xs of
       Just (_, x:_) -> return $ A.maybeResult $ A.parse parseSsPid $ BSC.pack x
       _ -> return Nothing
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

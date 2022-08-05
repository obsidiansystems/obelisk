{-# LANGUAGE CPP #-}
#if defined(IPROUTE_SUPPORTED)
{-# LANGUAGE TemplateHaskell #-}
#endif

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Due to instance HasJS x (EventWriterT t w m)
module Obelisk.Run where

import Prelude hiding ((.), id)

import Control.Category
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Lens ((%~), (^?), (?~), _Just, _Right)
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.UTF8 as BSUTF8
import Data.Functor.Identity
import Data.List (uncons)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup ((<>))
#endif
import Data.Streaming.Network (bindPortTCP)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import qualified Network.HTTP.ReverseProxy as RP
import qualified Network.HTTP.Types as H
import Network.Socket
import Network.Wai (Application)
import qualified Network.Wai as W
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.Warp.Internal (settingsHost, settingsPort)
import Network.WebSockets (ConnectionOptions)
import Network.WebSockets.Connection (defaultConnectionOptions)
import qualified Obelisk.Asset.Serve.Snap as Snap
import Obelisk.Backend
import Obelisk.Frontend
import Obelisk.Route.Frontend
import qualified OpenSSL.PEM as PEM
import qualified OpenSSL.RSA as RSA
import qualified OpenSSL.X509 as X509
import qualified OpenSSL.X509.Request as X509Request
import Reflex.Dom.Core
import Snap.Core (Snap)
import System.Environment
import System.FilePath ((</>))
import System.IO
import System.Process
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens
import Web.Cookie

#if defined(IPROUTE_SUPPORTED)
import qualified System.Which
#endif

-- | The arguments to 'run', specifying the configuration and
-- implementation of an Obelisk application.
data RunApp backendRoute frontendRoute
  = RunApp
    { _runApp_backendPort      :: Int
      -- ^ What port should we serve the backend on? This is used for
      -- internal communication.
    , _runApp_backendHost      :: ByteString
      -- ^ The hostname on which the backend is running. By default,
      -- this is @127.0.0.1@, i.e., the local machine. Routes not
      -- handled by the frontend will be redirected to this host.
    , _runApp_forceFrontendPort :: Maybe Int
      -- ^ If set, overrides the port on which the frontend will be
      -- served. If unset, the port will be parsed from the configured
      -- route.
    , _runApp_tlsCertDirectory :: Maybe FilePath
      -- ^ Optional directory in which to find "cert.pem", "chain.pem"
      -- and "privkey.pem" to be used for TLS.
      -- If this is 'Nothing' and TLS is enabled, we'll generate a
      -- self-signed cert.
    , _runApp_staticHandler    :: [Text] -> Snap ()
      -- ^ How to serve static assets.
    , _runApp_backend          :: Backend backendRoute frontendRoute
      -- ^ The backend.
    , _runApp_frontend         :: Frontend (R frontendRoute)
      -- ^ The frontend.
    }

-- | Construct a 'RunApp' with sane defaults. The TLS certificate
-- directory will be set to 'Nothing', the backend host will be the
-- local machine (@127.0.0.1@), the backend port will be set to @3001@,
-- the frontend port will be fetched from the route configuration.
defaultRunApp
  :: Backend backendRoute frontendRoute -- ^ The backend to use
  -> Frontend (R frontendRoute)         -- ^ The frontend to use
  -> ([Text] -> Snap ())                -- ^ How to serve static assets
  -> RunApp backendRoute frontendRoute
defaultRunApp be fe static = RunApp
  { _runApp_backendPort = 3001
  , _runApp_backendHost = "127.0.0.1"
  , _runApp_forceFrontendPort = Nothing
  , _runApp_tlsCertDirectory = Nothing
  , _runApp_staticHandler = static
  , _runApp_backend = be
  , _runApp_frontend = fe
  }

-- | Run an Obelisk application, including the frontend and backend. The
-- backend routes are served on the port given by '_runApp_backendPort',
-- but are also accessible through the frontend.
run :: RunApp backendRoute frontendRoute -> IO ()
run toRun = do
  prettifyOutput

  let handleBackendErr (e :: IOException) =
        hPutStrLn stderr $ "backend stopped; make a change to your code to reload - error " <> show e

  --TODO: Use Obelisk.Backend.runBackend; this will require separating the checking and running phases
  case checkEncoder $ _backend_routeEncoder (_runApp_backend toRun) of
    Left e -> hPutStrLn stderr $ "backend error:\n" <> T.unpack e
    Right validFullEncoder -> do
      publicConfigs <- getPublicConfigs

      -- We start the backend server listening on the
      -- '_runApp_backendPort'. The backend and frontend run in
      -- different servers: The frontend server will pass any routes it
      -- can't handle to this process.
      backendTid <- forkIO $ handle handleBackendErr $ withArgs ["--quiet", "--port", show (_runApp_backendPort toRun)] $
        _backend_run (_runApp_backend toRun) $ \serveRoute ->
          runSnapWithCommandLineArgs $
            getRouteWith validFullEncoder >>= \case
              Identity r -> case r of
                FullRoute_Backend backendRoute :/ a -> serveRoute $ backendRoute :/ a
                FullRoute_Frontend obeliskRoute :/ a ->
                  serveDefaultObeliskApp appRouteToUrl (($ allJsUrl) <$> defaultGhcjsWidgets)
                    (_runApp_staticHandler toRun) (_runApp_frontend toRun) publicConfigs $ obeliskRoute :/ a
                  where
                    appRouteToUrl (k :/ v) = renderObeliskRoute validFullEncoder (FullRoute_Frontend (ObeliskRoute_App k) :/ v)
                    allJsUrl = renderAllJsPath validFullEncoder

      runWidget toRun publicConfigs validFullEncoder `finally` killThread backendTid

-- Convenience wrapper to handle path segments for 'Snap.serveAsset'
runServeAsset :: FilePath -> [Text] -> Snap ()
runServeAsset rootPath t =
  Snap.serveAsset "" rootPath . T.unpack . T.intercalate "/" $ t

getConfigRoute :: Map Text ByteString -> Either Text URI
getConfigRoute configs = case Map.lookup "common/route" configs of
    Just r ->
      let stripped = T.strip (T.decodeUtf8 r)
      in case URI.mkURI stripped of
          Just route -> Right route
          Nothing -> Left $ "Couldn't parse route as URI; value read was: " <> T.pack (show stripped)
    Nothing -> Left $ "Couldn't find config file common/route; it should contain the site's canonical root URI" <> T.pack (show $ Map.keys configs)

-- | Start the frontend (given in the 'RunApp' record), with the given
-- configuration and the given 'FullRoute' encoder, which must be valid.
runWidget
  :: RunApp backendRoute frontendRoute
  -> Map Text ByteString
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName
  -> IO ()
runWidget toRun configs validFullEncoder = do
  uri <- either (fail . T.unpack) pure $ getConfigRoute configs
  let -- Before we can do anything, we need to pick a port to serve the
      -- backend on. If the user has asked to override it, then we use that:
      -- they know what they're doing. Otherwise, we'll use the port
      -- specified in the route.
      port = fromMaybe 80 $ (_runApp_forceFrontendPort toRun)
                        <|> (fmap fromIntegral $ uri ^? uriAuthority . _Right . authPort . _Just)
      -- This is the *actual* URI on which the frontend is served, i.e.
      -- the URI from the route configuration but, possibly, with the
      -- port we picked above. We need to compute this for two reasons:
      --
      --   1. The log. Self-explanatory.
      --   2. JSaddle needs to know where the frontend is served.
      actualUri = uri & uriAuthority . _Right . authPort ?~ fromInteger (fromIntegral port)

      -- This is the server that will handle the backend requests. We
      -- support shuttling them off to any host:port pair.
      redirectHost = _runApp_backendHost toRun
      redirectPort = _runApp_backendPort toRun

      beforeMainLoop = do
        putStrLn $ "Frontend running on " ++ T.unpack (URI.render actualUri)
      settings = setBeforeMainLoop beforeMainLoop (setPort port (setTimeout 3600 defaultSettings))

      -- Providing TLS here will also incidentally provide it to proxied requests to the backend.
      prepareRunner = case uri ^? uriScheme . _Just . unRText of
        Just "https" -> do
          case _runApp_tlsCertDirectory toRun of
            Nothing -> do
              -- Generate a private key and self-signed certificate for TLS
              privateKey <- RSA.generateRSAKey' 2048 3

              certRequest <- X509Request.newX509Req
              _ <- X509Request.setPublicKey certRequest privateKey
              _ <- X509Request.signX509Req certRequest privateKey Nothing

              cert <- X509.newX509 >>= X509Request.makeX509FromReq certRequest
              _ <- X509.setPublicKey cert privateKey
              timenow <- getCurrentTime
              _ <- X509.setNotBefore cert $ addUTCTime (-1) timenow
              _ <- X509.setNotAfter cert $ addUTCTime (365 * 24 * 60 * 60) timenow
              _ <- X509.signX509 cert privateKey Nothing

              certByteString <- BSUTF8.fromString <$> PEM.writeX509 cert
              privateKeyByteString <- BSUTF8.fromString <$> PEM.writePKCS8PrivateKey privateKey Nothing

              return $ runTLSSocket (tlsSettingsMemory certByteString privateKeyByteString)
            Just certDir -> do
              putStrLn $ "Using certificate information from: " ++ certDir
              return $ runTLSSocket (tlsSettingsChain (certDir </> "cert.pem") [certDir </> "chain.pem"] (certDir </> "key.pem"))
        _ -> return runSettingsSocket

  runner <- prepareRunner
  bracket
    (bindPortTCPRetry settings (logPortBindErr port) 1)
    close
    (\skt -> do
        man <- newManager defaultManagerSettings
        app <- obeliskApp configs defaultConnectionOptions (_runApp_frontend toRun) validFullEncoder actualUri $ fallbackProxy redirectHost redirectPort man
        runner settings skt app)


-- | Build a WAI 'Application' to serve the given Obelisk 'Frontend',
-- using the specified 'Encoder' to parse routes. Any requests whose
-- route does not result in a 'FullRoute_Frontend' parse will be
-- redirected to the backend.
obeliskApp
  :: forall frontendRoute backendRoute
  .  Map Text ByteString -- ^ The parsed configuration
  -> ConnectionOptions   -- ^ Connection options for the JSaddle websocket
  -> Frontend (R frontendRoute) -- ^ The Obelisk frontend
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName
     -- ^ An encoder for parsing frontend routes.
  -> URI
    -- ^ The 'URI' on which the 'Frontend' will be served. Used for
    -- establishing the JSaddle websocket connection.
  -> Application -- ^ A WAI 'Application' which handles backend requests.
  -> IO Application
obeliskApp configs opts frontend validFullEncoder uri backend = do
  let mode = FrontendMode
        { _frontendMode_hydrate = True
        , _frontendMode_adjustRoute = False
        }
      entryPoint = do
        runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend
        syncPoint
  jsaddlePath <- URI.mkPathPiece "jsaddle"
  let jsaddleUri = BSLC.fromStrict $ URI.renderBs $ uri & uriPath %~ (<>[jsaddlePath])
  Right (jsaddleWarpRouteValidEncoder :: Encoder Identity (Either Text) (R JSaddleWarpRoute) PageName) <- return $ checkEncoder jsaddleWarpRouteEncoder
  jsaddle <- jsaddleWithAppOr opts entryPoint $ \_ sendResponse -> sendResponse $ W.responseLBS H.status500 [("Content-Type", "text/plain")] "obeliskApp: jsaddle got a bad URL"
  return $ \req sendResponse -> case tryDecode validFullEncoder $ byteStringsToPageName (BS.dropWhile (== (fromIntegral $ fromEnum '/')) $ W.rawPathInfo req) (BS.drop 1 $ W.rawQueryString req) of
    Identity r -> case r of
      FullRoute_Frontend (ObeliskRoute_Resource ResourceRoute_JSaddleWarp) :/ jsaddleRoute -> case jsaddleRoute of
        JSaddleWarpRoute_JavaScript :/ () -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] $ jsaddleJs' (Just jsaddleUri) False
        _ -> flip jsaddle sendResponse $ req
          { W.pathInfo = fst $ encode jsaddleWarpRouteValidEncoder jsaddleRoute
          }
      FullRoute_Frontend (ObeliskRoute_App appRouteComponent) :/ appRouteRest -> do
        let cookies = maybe [] parseCookies $ lookup (fromString "Cookie") (W.requestHeaders req)
            routeToUrl (k :/ v) = renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_App k) :/ v
        html <- renderJsaddleFrontend configs cookies routeToUrl (appRouteComponent :/ appRouteRest) frontend
        sendResponse $ W.responseLBS H.status200 [("Content-Type", staticRenderContentType)] $ BSLC.fromStrict html
      _ -> backend req sendResponse

renderJsaddleFrontend
  :: Map Text ByteString
  -> Cookies
  -> (route -> Text)
  -> route
  -> Frontend route
  -> IO ByteString
renderJsaddleFrontend configs cookies urlEnc r f =
  let jsaddleScript = elAttr "script" ("src" =: "/jsaddle/jsaddle.js") blank
      jsaddlePreload = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: "/jsaddle/jsaddle.js") blank
  in renderFrontendHtml configs cookies urlEnc r f jsaddlePreload jsaddleScript

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

ssPath :: Maybe String
ssPath =
#if defined(IPROUTE_SUPPORTED)
  Just $(System.Which.staticWhich "ss")
#else
  Nothing
#endif

getProcessIdForPort :: Int -> IO (Maybe Int)
getProcessIdForPort port = case ssPath of
  Just ss -> do
    xs <- lines <$> readProcess ss ["-lptn", "sport = " <> show port] mempty
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

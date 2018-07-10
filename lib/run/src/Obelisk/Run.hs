{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Obelisk.Run where

import Prelude hiding ((.), id)

import Control.Category
import Control.Concurrent
import Control.Exception
import Control.Lens ((^?), _Just, _Right)
import Control.Monad.Except
import Control.Monad.Ref
import qualified Data.Attoparsec.ByteString.Char8 as A
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Dependent.Sum (DSum (..))
import Data.Functor.Identity
import Data.IORef
import Data.List (uncons)
import Data.Maybe
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.Semigroup ((<>))
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Streaming.Network (bindPortTCP)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Universe
import GHCJS.DOM hiding (bracket, catch)
import GHCJS.DOM.Document
import GHCJS.DOM.Node
import qualified GHCJS.DOM.Types as DOM
import Language.Javascript.JSaddle (JSM)
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
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import System.Environment
import System.IO
import System.Process
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens

run
  :: ( Universe route
     , Ord route
     , Show route
     )
  => Int -- ^ Port to run the backend
  -> Either Text (IO ()) -- ^ Backend
  -> Frontend route -- ^ Frontend
  -> IO ()
run port checkBackend frontend = do
  let handleBackendErr (e :: IOException) = hPutStrLn stderr $ "backend stopped; make a change to your code to reload - error " <> show e
  case checkBackend of
    Left e -> hPutStrLn stderr $ "backend error:\n" <> T.unpack e
    Right backend -> do
      backendTid <- forkIO $ handle handleBackendErr $ withArgs ["--quiet", "--port", show port] backend
      let conf = defRunConfig { _runConfig_redirectPort = port }
      runWidget conf frontend `finally` killThread backendTid

getConfigRoute :: IO (Maybe URI)
getConfigRoute = get "common/route" >>= \case
  Just r -> case URI.mkURI $ T.strip r of
    Just route -> pure $ Just route
    Nothing -> do
      putStrLn $ "Route is invalid: " <> show r
      pure Nothing
  Nothing -> pure Nothing

defAppUri :: URI
defAppUri = fromMaybe (error "defAppUri") $ URI.mkURI "http://127.0.0.1:8000"

runWidget :: (Universe route, Ord route, Show route) => RunConfig -> Frontend route -> IO ()
runWidget conf frontend = do
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
        app <- obeliskApp defaultConnectionOptions frontend (fallbackProxy redirectHost redirectPort man)
        runSettingsSocket settings skt app)

data Void1 :: * -> * where {}

instance Universe (Some Void1) where
  universe = []

void1Encoder :: (Applicative check, MonadError Text parse) => Encoder check parse (Some Void1) a
void1Encoder = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = \case
      Some.This f -> case f of {}
  , _validEncoder_decode = \_ -> throwError "void1Encoder: can't decode anything"
  }

type Widget' x = ImmediateDomBuilderT DomTimeline (PostBuildT DomTimeline (WithJSContextSingleton x (PerformEventT DomTimeline DomHost))) --TODO: Make this more abstract

{-# INLINABLE attachWidget''' #-}
attachWidget''' :: (Ref m ~ Ref IO, MonadIO m, MonadReflexHost DomTimeline m, MonadRef m) => (EventChannel -> PerformEventT DomTimeline m (a, IORef (Maybe (EventTrigger DomTimeline ())))) -> m (a, FireCommand DomTimeline m)
attachWidget''' w = do
  events <- liftIO newChan
  ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ w events
  mPostBuildTrigger <- readRef postBuildTriggerRef
  forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
  return (result, fc)

-- | Warning: `mainWidgetWithHead'` is provided only as performance tweak. It is expected to disappear in future releases.
runFrontend :: (a -> Widget' () b, b -> Widget' () a) -> JSM ()
runFrontend widgets = withJSContextSingletonMono $ \jsSing -> do
  doc <- currentDocumentUnchecked
  headElement <- getHeadUnchecked doc
  headFragment <- createDocumentFragment doc
  bodyElement <- getBodyUnchecked doc
  bodyFragment <- createDocumentFragment doc
  (events, fc) <- liftIO $ runDomHost $ attachWidget''' $ \events -> flip runWithJSContextSingleton jsSing $ do
    let (headWidget, bodyWidget) = widgets
    (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
    let go :: forall c. Widget' () c -> DOM.DocumentFragment -> PostBuildT DomTimeline (WithJSContextSingleton () (PerformEventT DomTimeline DomHost)) c
        go w df = do
          unreadyChildren <- liftIO $ newIORef 0
          let builderEnv = ImmediateDomBuilderEnv
                { _immediateDomBuilderEnv_document = toDocument doc
                , _immediateDomBuilderEnv_parent = toNode df
                , _immediateDomBuilderEnv_unreadyChildren = unreadyChildren
                , _immediateDomBuilderEnv_commitAction = return () --TODO
                }
          runImmediateDomBuilderT w builderEnv events
    flip runPostBuildT postBuild $ do
      rec b <- go (headWidget a) headFragment
          a <- go (bodyWidget b) bodyFragment
      return (events, postBuildTriggerRef)
  replaceElementContents headElement headFragment
  replaceElementContents bodyElement bodyFragment
  liftIO $ processAsyncEvents events fc

--instance DOM.MonadJSM m => DOM.MonadJSM (PerformEventT t m)

-- obeliskRouteEncoder routeComponentEncoder routeRestEncoder . Encoder (pure $ prismValidEncoder $ rPrism _ObeliskRoute_App) --TODO: Deal with failure

obeliskApp :: forall route. (Universe route, Ord route, Show route) => ConnectionOptions -> Frontend route -> Application -> IO Application
obeliskApp opts frontend backend = do
  html <- BSLC.fromStrict <$> indexHtml blank --TODO: Something other than `blank` here?
  let runMyRouteViewT = runRouteViewT
        (_frontend_routeEncoder frontend)
        (_frontend_title frontend)
        (_frontend_notFoundRoute frontend)
  let entryPoint = runFrontend (\_ -> runMyRouteViewT $ _frontend_head frontend, \_ -> runMyRouteViewT $ _frontend_body frontend) >> syncPoint
  Right ve <- return $ checkEncoder $ obeliskRouteEncoder void1Encoder (\case {})
  Right (jsaddleWarpRouteValidEncoder :: ValidEncoder (Either Text) (R JSaddleWarpRoute) PageName) <- return $ checkEncoder jsaddleWarpRouteEncoder
  jsaddle <- jsaddleWithAppOr opts entryPoint $ error "obeliskApp: jsaddle got a bad URL"
  return $ \req sendResponse -> case _validEncoder_decode ve (W.pathInfo req, mempty) of --TODO: Query strings
    Right (ObeliskRoute_Resource ResourceRoute_JSaddleWarp :/ jsaddleRoute) -> case jsaddleRoute of
      JSaddleWarpRoute_JavaScript :/ () -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "application/javascript")] $ jsaddleJs' (Just "http://localhost:8000/jsaddle") False
      _ -> flip jsaddle sendResponse $ req
        { W.pathInfo = fst $ _validEncoder_encode jsaddleWarpRouteValidEncoder jsaddleRoute
        }
    Left _ -> sendResponse $ W.responseLBS H.status200 [("Content-Type", "text/html")] html
    _ -> backend req sendResponse

indexHtml :: StaticWidget () () -> IO ByteString
indexHtml h = do
  ((), bs) <- renderStatic $ el "html" $ do
    el "head" $ h
    el "body" $ return ()
    elAttr "script" ("src" =: "/jsaddle/jsaddle.js") $ return ()
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

deriveGCompare ''Void1
deriveGEq ''Void1
instance GShow Void1 where
  gshowsPrec _ = \case {}

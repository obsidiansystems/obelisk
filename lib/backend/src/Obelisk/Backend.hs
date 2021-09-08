{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Obelisk.Backend
  ( Backend (..)
  , BackendConfig (..)
  , defaultBackendConfig
  , StaticAssets (..)
  , defaultStaticAssets
  -- * Running a backend
  , runBackend
  , runBackendWith
  -- * Configuration of backend
  , GhcjsWidgets(..)
  , defaultGhcjsWidgets
  -- * all.js script loading functions
  , deferredGhcjsScript
  , delayedGhcjsScript
  -- * all.js preload functions
  , preloadGhcjs
  , renderAllJsPath
  -- * Re-exports
  , Default (def)
  , getPageName
  , requestDomain
  , requestPageName
  , getRouteWith
  , runSnapWithCommandLineArgs
  , runSnapWithConfig
  , serveDefaultObeliskApp
  , prettifyOutput
  , staticRenderContentType
  , getPublicConfigs
  , SomeDomain(..)
  , CheckedEncoders(..)
  , checkAllEncoders
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import qualified Data.Dependent.Map as DMap
import Data.Functor.Identity
import Data.GADT.Compare
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Universe
import GHC.Generics (Generic)
import Network.URI (URI)
import Obelisk.Asset.Serve.Snap (serveAsset)
import qualified Obelisk.ExecutableConfig.Lookup as Lookup
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Snap.Extras (doNotCache, serveFileIfExistsAs)
import Reflex.Dom.Core hiding (Request)
import Snap (MonadSnap, Snap, Request, commandLineConfig, defaultConfig, getsRequest, httpServe, modifyResponse
            , getRequest, rqHostName, rqPathInfo, rqQueryString, setContentType, writeBS, writeText
            , rqCookies, Cookie(..) , setHeader)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend route = Backend
  --{ _backend_routeEncoder :: RouteConfig -> Encoder (Either Text) Identity (R route) DomainPageName
  { _backend_routeEncoder :: forall b f. route (R (FullRoute b f)) -> Encoder (Either Text) Identity (R (FullRoute b f)) PageName
  , _backend_obRunBaseRoute :: forall a. RouteConfig -> route a -> URI
  , _backend_run :: ((forall b f. route (R (FullRoute b f)) -> R b -> Snap ()) -> IO ()) -> IO ()
  , _backend_frontend :: forall b f. route (R (FullRoute b f)) -> Frontend (R f)
  , _backend_frontendName :: forall a. route a -> String -- Name of the frontend library, for finding GHCJS assets
  }

data BackendConfig = BackendConfig
  { _backendConfig_runSnap :: !(Snap () -> IO ()) -- ^ Function to run the snap server
  , _backendConfig_staticAssets :: !StaticAssets -- ^ Static assets
  , _backendConfig_ghcjsWidgets :: !(GhcjsWidgets (Text -> FrontendWidgetTInner ()))
    -- ^ Given the URL of all.js, return the widgets which are responsible for
    -- loading the script.
  } deriving (Generic)

-- | The static assets provided must contain a compiled GHCJS app that corresponds exactly to the Frontend provided
data GhcjsApp route = GhcjsApp
  { _ghcjsApp_compiled :: !StaticAssets
  , _ghcjsApp_value :: !(Frontend route)
  } deriving (Generic)

-- | Widgets used to load all.js on the frontend
data GhcjsWidgets a = GhcjsWidgets
  { _ghcjsWidgets_preload :: a
  -- ^ A preload widget, placed in the document head
  , _ghcjsWidgets_script :: a
  -- ^ A script widget, placed in the document body
  } deriving (Functor, Generic)


-- | Given the URL of all.js, return the widgets which are responsible for
-- loading the script. Defaults to 'preloadGhcjs' and 'deferredGhcjsScript'.
defaultGhcjsWidgets :: GhcjsWidgets (Text -> FrontendWidgetTInner ())
defaultGhcjsWidgets = GhcjsWidgets
  { _ghcjsWidgets_preload = preloadGhcjs
  , _ghcjsWidgets_script = deferredGhcjsScript
  }

-- | Serve a frontend, which must be the same frontend that Obelisk has built and placed in the default location
--TODO: The frontend should be provided together with the asset paths so that this isn't so easily breakable; that will probably make this function obsolete
serveDefaultObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute -> Text)
  -> GhcjsWidgets (FrontendWidgetTInner ())
  -> ([Text] -> m ())
  -> Frontend (R appRoute)
  -> String
  -- ^ Name of the frontend executable
  -> Map Text ByteString
  -> R (ObeliskRoute appRoute)
  -> m ()
serveDefaultObeliskApp urlEnc ghcjsWidgets serveStaticAsset frontend frontendName =
  serveObeliskApp urlEnc ghcjsWidgets serveStaticAsset frontendApp
  where frontendApp = GhcjsApp
          { _ghcjsApp_compiled = mkFrontendGhcjsAssets frontendName
          , _ghcjsApp_value = frontend
          }

prettifyOutput :: IO ()
prettifyOutput = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

defaultStaticAssets :: StaticAssets
defaultStaticAssets = StaticAssets
  { _staticAssets_processed = "static.assets"
  , _staticAssets_unprocessed = "static"
  }

mkFrontendGhcjsAssets :: String -> StaticAssets
mkFrontendGhcjsAssets frontendName = StaticAssets
  { _staticAssets_processed = frontendName <> ".jsexe.assets"
  , _staticAssets_unprocessed = frontendName <> ".jsexe"
  }

runSnapWithConfig :: MonadIO m => Config Snap a -> Snap () -> m ()
runSnapWithConfig conf a = do
  let httpConf = conf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  liftIO $ httpServe httpConf a

-- Get the web server configuration from the command line
runSnapWithCommandLineArgs :: MonadIO m => Snap () -> m ()
runSnapWithCommandLineArgs s = liftIO (commandLineConfig defaultConfig) >>= \c ->
  runSnapWithConfig c s

getPageName :: (MonadSnap m) => m PageName
getPageName = do
  r <- getRequest
  pure $ requestPageName r

getDomainPageName :: (MonadSnap m) => m DomainPageName
getDomainPageName = do
  r <- getRequest
  pure (requestDomain r, requestPageName r)

requestPageName :: Request -> PageName
requestPageName r = byteStringsToPageName p q
  where
    p = rqPathInfo r
    q = rqQueryString r

-- | Turn a request into a protocol relative domain
requestDomain :: Request -> Domain
requestDomain req = Domain $ "//" <> T.decodeUtf8 (rqHostName req)

getRouteWith
  :: MonadSnap m
  => (Domain -> SomeDomain p)
  -> (forall b f. p (R (FullRoute b f)) -> Encoder Identity parse (R (FullRoute b f)) PageName)
  -> (forall b f. p (R (FullRoute b f)) -> parse (R (FullRoute b f)) -> m x)
  -> m x
getRouteWith parseDomain mkEncoder handler = do
  (domain, pageName) <- getDomainPageName
  case parseDomain domain of
    SomeDomain p -> handler p $ tryDecode (mkEncoder p) pageName

renderAllJsPath :: Encoder Identity Identity (R (FullRoute a b)) PageName -> Text
renderAllJsPath validFullEncoder =
  renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_Resource ResourceRoute_Ghcjs) :/ ["all.js"]

serveObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute -> Text)
  -> GhcjsWidgets (FrontendWidgetTInner ())
  -> ([Text] -> m ())
  -> GhcjsApp (R appRoute)
  -> Map Text ByteString
  -> R (ObeliskRoute appRoute)
  -> m ()
serveObeliskApp urlEnc ghcjsWidgets serveStaticAsset frontendApp config = \case
  ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> serveGhcjsApp urlEnc ghcjsWidgets frontendApp config $ GhcjsAppRoute_App appRouteComponent :/ appRouteRest
  ObeliskRoute_Resource resComponent :=> Identity resRest -> case resComponent :=> Identity resRest of
    ResourceRoute_Static :=> Identity pathSegments -> serveStaticAsset pathSegments
    ResourceRoute_Ghcjs :=> Identity pathSegments -> serveGhcjsApp urlEnc ghcjsWidgets frontendApp config $ GhcjsAppRoute_Resource :/ pathSegments
    ResourceRoute_JSaddleWarp :=> Identity _ -> do
      let msg = "Error: Obelisk.Backend received jsaddle request"
      liftIO $ putStrLn $ T.unpack msg
      writeText msg
    ResourceRoute_Version :=> Identity () -> doNotCache >> serveFileIfExistsAs "text/plain" "version"

serveStaticAssets :: (MonadSnap m, MonadFail m) => StaticAssets -> [Text] -> m ()
serveStaticAssets assets pathSegments = serveAsset (_staticAssets_processed assets) (_staticAssets_unprocessed assets) $ T.unpack $ T.intercalate "/" pathSegments

data StaticAssets = StaticAssets
  { _staticAssets_processed :: !FilePath
  , _staticAssets_unprocessed :: !FilePath
  }
  deriving (Show, Read, Eq, Ord)

data GhcjsAppRoute :: (* -> *) -> * -> * where
  GhcjsAppRoute_App :: appRouteComponent a -> GhcjsAppRoute appRouteComponent a
  GhcjsAppRoute_Resource :: GhcjsAppRoute appRouteComponent [Text]

staticRenderContentType :: ByteString
staticRenderContentType = "text/html; charset=utf-8"

--TODO: Don't assume we're being served at "/"
serveGhcjsApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRouteComponent -> Text)
  -> GhcjsWidgets (FrontendWidgetTInner ())
  -> GhcjsApp (R appRouteComponent)
  -> Map Text ByteString
  -> R (GhcjsAppRoute appRouteComponent)
  -> m ()
serveGhcjsApp urlEnc ghcjsWidgets app config = \case
  GhcjsAppRoute_App appRouteComponent :=> Identity appRouteRest -> do
    modifyResponse $ setContentType staticRenderContentType
    modifyResponse $ setHeader "Cache-Control" "no-store private"
    writeBS <=< renderGhcjsFrontend urlEnc ghcjsWidgets (appRouteComponent :/ appRouteRest) config $ _ghcjsApp_value app
  GhcjsAppRoute_Resource :=> Identity pathSegments -> serveStaticAssets (_ghcjsApp_compiled app) pathSegments

-- | Default obelisk backend configuration.
defaultBackendConfig :: BackendConfig
defaultBackendConfig = BackendConfig runSnapWithCommandLineArgs defaultStaticAssets defaultGhcjsWidgets

-- | Run an obelisk backend with the default configuration.
runBackend :: (GCompare route, Universe (SomeDomain route)) => Backend route -> IO ()
runBackend = runBackendWith defaultBackendConfig

-- | Run an obelisk backend with the given configuration.
-- Full encoder implementation. Falls down proving route a ~ route (R (FullRoute b f)), I couldn't figure out how to make it work with ArgDict.
-- runBackendWith'
--  :: BackendConfig
--  -> Backend route
--  -> IO ()
-- runBackendWith' (BackendConfig runSnap staticAssets ghcjsWidgets) backend = do
--  publicConfigs <- getPublicConfigs
--  let routeConfig = getCheckedRouteConfig publicConfigs
--  case checkEncoder $ _backend_routeEncoder backend routeConfig of
--    Left e -> fail $ "backend error:\n" <> T.unpack e
--    Right (validFullEncoder :: Encoder Identity Identity (R route) DomainPageName) -> do
--      _backend_run backend $ \serveRoute -> do
--        runSnap $
--          getRouteWith validFullEncoder $ \outerRoute -> \case
--            (innerRoute :: R (FullRoute b f)) -> case innerRoute of
--              FullRoute_Backend backendRoute :/ a -> do
--                liftIO $ putStrLn "backendRoute"
--                serveRoute outerRoute $ backendRoute :/ a
--              FullRoute_Frontend obeliskRoute :/ a ->
--                serveDefaultObeliskApp
--                  routeToUrl
--                  (($ allJsUrl) <$> ghcjsWidgets)
--                  (serveStaticAssets staticAssets)
--                  (_backend_frontend backend outerRoute)
--                  publicConfigs
--                  (obeliskRoute :/ a)
--                where
--                  routeToUrl (k :/ v) = renderFullObeliskRoute validFullEncoder $ outerRoute :/ FullRoute_Frontend (ObeliskRoute_App k) :/ v
--                  allJsUrl = renderAllJsPath validFullEncoder outerRoute

-- Essentially 'Flip'
newtype Enc a = Enc (Encoder Identity Identity a PageName)
newtype CheckedEncoders route = CheckedEncoders (forall b f. route (R (FullRoute b f)) -> Encoder Identity Identity (R (FullRoute b f)) PageName)

checkAllEncoders
  :: (Universe (SomeDomain route), GCompare route)
  => (forall b f. route (R (FullRoute b f)) -> Encoder (Either Text) Identity (R (FullRoute b f)) PageName)
  -> Either Text (CheckedEncoders route)
checkAllEncoders mkEncoder = do
  encs <- traverse (\(SomeDomain r) -> (\a -> r :=> Enc a) <$> checkEncoder (mkEncoder r)) universe
  let encMap = DMap.fromList encs
  pure $ CheckedEncoders $ \r -> case DMap.lookup r encMap of
    Nothing -> error "checkAllEncoders: couldn't find encoder in map, should be impossible. The universe instance must not be a true universe instance"
    Just (Enc e) -> e

-- | Run an obelisk backend with the given configuration.
runBackendWith
  :: forall route. (GCompare route, Universe (SomeDomain route))
  => BackendConfig
  -> Backend route
  -> IO ()
runBackendWith (BackendConfig runSnap staticAssets ghcjsWidgets) backend = do
  publicConfigs <- getPublicConfigs
  case checkAllEncoders $ _backend_routeEncoder backend of
    Left e -> fail $ "backend error:\n" <> T.unpack e
    Right (CheckedEncoders mkValidEncoder) -> do
      let routeConfig = getCheckedRouteConfig publicConfigs
          backwardsURI :: Map (Maybe Domain) (SomeDomain route)
          backwardsURI = Map.fromList $ (\(SomeDomain route) -> (uriToDomain $ _backend_obRunBaseRoute backend routeConfig route, SomeDomain route)) <$> universe
          parseDomain :: Domain -> SomeDomain route
          parseDomain d = case Map.lookup (Just d) backwardsURI of
              Nothing -> error $ "parseDomain: couln't find URI: " <> show d
              Just someDomain -> someDomain
      _backend_run backend $ \serveRoute -> do
        runSnap $
          getRouteWith parseDomain mkValidEncoder $ \domainPart -> \case
            Identity (r :: R (FullRoute b f)) -> case r of
              FullRoute_Backend backendRoute :/ a -> do
                liftIO $ putStrLn "backendRoute"
                serveRoute domainPart $ backendRoute :/ a
              FullRoute_Frontend obeliskRoute :/ a ->
                serveDefaultObeliskApp
                  routeToUrl
                  (($ allJsUrl) <$> ghcjsWidgets)
                  (serveStaticAssets staticAssets)
                  (_backend_frontend backend domainPart)
                  (_backend_frontendName backend domainPart)
                  publicConfigs
                  (obeliskRoute :/ a)
                where
                  routeToUrl (k :/ v) = renderObeliskRoute (mkValidEncoder domainPart) $ FullRoute_Frontend (ObeliskRoute_App k) :/ v
                  allJsUrl = renderAllJsPath (mkValidEncoder domainPart)

renderGhcjsFrontend
  :: (MonadSnap m, HasCookies m)
  => (route -> Text)
  -> GhcjsWidgets (FrontendWidgetTInner ())
  -> route
  -> Map Text ByteString
  -> Frontend route
  -> m ByteString
renderGhcjsFrontend urlEnc ghcjsWidgets route configs f = do
  cookies <- askCookies
  renderFrontendHtml configs cookies urlEnc route f
    (lift $ lift $ lift $ _ghcjsWidgets_preload ghcjsWidgets)
    (lift $ lift $ lift $ _ghcjsWidgets_script ghcjsWidgets)

-- | Preload all.js in a link tag.
-- This is the default preload method.
preloadGhcjs :: DomBuilder t m => Text -> m ()
preloadGhcjs allJsUrl = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: allJsUrl) blank

-- | Load the script from the given URL in a deferred script tag.
-- This is the default method.
deferredGhcjsScript :: DomBuilder t m => Text -> m ()
deferredGhcjsScript allJsUrl = elAttr "script" ("type" =: "text/javascript" <> "src" =: allJsUrl <> "defer" =: "defer") blank

-- | An all.js script which is loaded after waiting for some time to pass. This
-- is useful to ensure any CSS animations on the page can play smoothly before
-- blocking the UI thread by running all.js.
delayedGhcjsScript
  :: Int -- ^ The number of milliseconds to delay loading by
  -> Text -- ^ URL to GHCJS app JavaScript
  -> FrontendWidgetT r ()
delayedGhcjsScript n allJsUrl = elAttr "script" ("type" =: "text/javascript") $ text $ T.unlines
  [ "setTimeout(function() {"
  , "  var all_js_script = document.createElement('script');"
  , "  all_js_script.type = 'text/javascript';"
  , "  all_js_script.src = '" <> allJsUrl <> "';"
  , "  document.body.appendChild(all_js_script);"
  , "}, " <> T.pack (show n) <> ");"
  ]

instance HasCookies Snap where
  askCookies = map (\c -> (cookieName c, cookieValue c)) <$> getsRequest rqCookies

-- | Get configs from the canonical "public" locations (i.e., locations that obelisk expects to make available
-- to frontend applications, and hence visible to end users).
getPublicConfigs :: IO (Map Text ByteString)
getPublicConfigs = Map.filterWithKey (\k _ -> isMemberOf k ["common", "frontend"]) <$> Lookup.getConfigs
  where
    isMemberOf k = any (`T.isPrefixOf` k)

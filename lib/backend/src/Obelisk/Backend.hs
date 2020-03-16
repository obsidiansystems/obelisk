{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Obelisk.Backend
  ( Backend (..)
  , StaticAssets (..)
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
  , getRouteWith
  , runSnapWithCommandLineArgs
  , runSnapWithConfig
  , serveDefaultObeliskApp
  , prettifyOutput
  , staticRenderContentType
  , getPublicConfigs
  ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Asset.Serve.Snap (serveAsset)
import qualified Obelisk.ExecutableConfig.Lookup as Lookup
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Snap.Extras (doNotCache, serveFileIfExistsAs)
import Reflex.Dom
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, modifyResponse
            , rqPathInfo, rqQueryString, setContentType, writeBS, writeText
            , rqCookies, Cookie(..) , setHeader)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) Identity (R (FullRoute backendRoute frontendRoute)) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  }

data BackendConfig frontendRoute = BackendConfig
  { _backendConfig_runSnap :: !(Snap () -> IO ()) -- ^ Function to run the snap server
  , _backendConfig_staticAssets :: !StaticAssets -- ^ Static assets
  , _backendConfig_ghcjsWidgets :: !(GhcjsWidgets (Text -> FrontendWidgetT (R frontendRoute) ()))
    -- ^ Given the URL of all.js, return the widgets which are responsible for
    -- loading the script.
  }

-- | The static assets provided must contain a compiled GHCJS app that corresponds exactly to the Frontend provided
data GhcjsApp route = GhcjsApp
  { _ghcjsApp_compiled :: !StaticAssets
  , _ghcjsApp_value :: !(Frontend route)
  }

-- | Widgets used to load all.js on the frontend
data GhcjsWidgets a = GhcjsWidgets
  { _ghcjsWidgets_preload :: a
  -- ^ A preload widget, placed in the document head
  , _ghcjsWidgets_script :: a
  -- ^ A script widget, placed in the document body
  } deriving Functor


-- | Given the URL of all.js, return the widgets which are responsible for
-- loading the script. Defaults to 'preloadGhcjs' and 'deferredGhcjsScript'.
defaultGhcjsWidgets :: GhcjsWidgets (Text -> FrontendWidgetT r ())
defaultGhcjsWidgets = GhcjsWidgets
  { _ghcjsWidgets_preload = preloadGhcjs
  , _ghcjsWidgets_script = deferredGhcjsScript
  }

-- | Serve a frontend, which must be the same frontend that Obelisk has built and placed in the default location
--TODO: The frontend should be provided together with the asset paths so that this isn't so easily breakable; that will probably make this function obsolete
serveDefaultObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute -> Text)
  -> GhcjsWidgets (FrontendWidgetT (R appRoute) ())
  -> ([Text] -> m ())
  -> Frontend (R appRoute)
  -> Map Text ByteString
  -> R (ObeliskRoute appRoute)
  -> m ()
serveDefaultObeliskApp urlEnc ghcjsWidgets serveStaticAsset frontend =
  serveObeliskApp urlEnc ghcjsWidgets serveStaticAsset frontendApp
  where frontendApp = GhcjsApp
          { _ghcjsApp_compiled = defaultFrontendGhcjsAssets
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

defaultFrontendGhcjsAssets :: StaticAssets
defaultFrontendGhcjsAssets = StaticAssets
  { _staticAssets_processed = "frontend.jsexe.assets"
  , _staticAssets_unprocessed = "frontend.jsexe"
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
  p <- getsRequest rqPathInfo
  q <- getsRequest rqQueryString
  return $ byteStringsToPageName p q

getRouteWith :: (MonadSnap m) => Encoder Identity parse route PageName -> m (parse route)
getRouteWith e = do
  pageName <- getPageName
  return $ tryDecode e pageName

renderAllJsPath :: Encoder Identity Identity (R (FullRoute a b)) PageName -> Text
renderAllJsPath validFullEncoder =
  renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_Resource ResourceRoute_Ghcjs) :/ ["all.js"]

serveObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute -> Text)
  -> GhcjsWidgets (FrontendWidgetT (R appRoute) ())
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
  -> GhcjsWidgets (FrontendWidgetT (R appRouteComponent) ())
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
defaultBackendConfig :: BackendConfig frontendRoute
defaultBackendConfig = BackendConfig runSnapWithCommandLineArgs defaultStaticAssets defaultGhcjsWidgets

-- | Run an obelisk backend with the default configuration.
runBackend :: Backend backendRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runBackend = runBackendWith defaultBackendConfig

-- | Run an obelisk backend with the given configuration.
runBackendWith
  :: BackendConfig frontendRoute
  -> Backend backendRoute frontendRoute
  -> Frontend (R frontendRoute)
  -> IO ()
runBackendWith (BackendConfig runSnap staticAssets ghcjsWidgets) backend frontend = case checkEncoder $ _backend_routeEncoder backend of
  Left e -> fail $ "backend error:\n" <> T.unpack e
  Right validFullEncoder -> do
    publicConfigs <- getPublicConfigs
    _backend_run backend $ \serveRoute ->
      runSnap $
        getRouteWith validFullEncoder >>= \case
          Identity r -> case r of
            FullRoute_Backend backendRoute :/ a -> serveRoute $ backendRoute :/ a
            FullRoute_Frontend obeliskRoute :/ a ->
              serveDefaultObeliskApp routeToUrl (($ allJsUrl) <$> ghcjsWidgets) (serveStaticAssets staticAssets) frontend publicConfigs $
                obeliskRoute :/ a
              where
                routeToUrl (k :/ v) = renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_App k) :/ v
                allJsUrl = renderAllJsPath validFullEncoder

renderGhcjsFrontend
  :: (MonadSnap m, HasCookies m)
  => (route -> Text)
  -> GhcjsWidgets (FrontendWidgetT route ())
  -> route
  -> Map Text ByteString
  -> Frontend route
  -> m ByteString
renderGhcjsFrontend urlEnc ghcjsWidgets route configs f = do
  cookies <- askCookies
  renderFrontendHtml configs cookies urlEnc route f (_ghcjsWidgets_preload ghcjsWidgets) (_ghcjsWidgets_script ghcjsWidgets)

-- | Preload all.js in a link tag.
-- This is the default preload method.
preloadGhcjs :: Text -> FrontendWidgetT r ()
preloadGhcjs allJsUrl = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: allJsUrl) blank

-- | Load the script from the given URL in a deferred script tag.
-- This is the default method.
deferredGhcjsScript :: Text -> FrontendWidgetT r ()
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

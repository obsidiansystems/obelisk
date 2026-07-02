{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
  , BackendConfig (..)
  , defaultBackendConfig
  , StaticAssets (..)
  , defaultStaticAssets
  -- * Running a backend
  , runBackend
  , runBackendWith
  -- * Configuration of backend
  , GhcjsWidgets(..)
  , GhcjsAppUrls(..)
  , defaultGhcjsWidgets
  -- * all.js script loading functions
  , deferredGhcjsScript
  , delayedGhcjsScript
  -- * all.js preload functions
  , preloadGhcjs
  , preloadWasm
  , renderAllJsPath
  , renderFrontendWasmPath
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

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Control.Monad.Fail (MonadFail)
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ >= 906
import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
#else
import Control.Monad.Except
#endif
#endif

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Kind (Type)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Obelisk.Asset.Serve.Snap (serveAsset)
import qualified Obelisk.ExecutableConfig.Lookup as Lookup
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Snap.Extras (doNotCache, serveFileIfExistsAs)
import Reflex.Dom.Core
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, modifyResponse
            , rqPathInfo, rqQueryString, setContentType, writeBS, writeText
            , rqCookies, Cookie(..) , setHeader)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.Directory (doesPathExist)
import System.FilePath ((</>))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) Identity (R (FullRoute backendRoute frontendRoute)) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  } deriving (Generic)

data BackendConfig frontendRoute = BackendConfig
  { _backendConfig_runSnap :: !(Snap () -> IO ()) -- ^ Function to run the snap server
  , _backendConfig_staticAssets :: !StaticAssets -- ^ Static assets
  , _backendConfig_frontendGhcjsAssets :: !StaticAssets -- ^ Compiled GHCJS frontend assets
  , _backendConfig_ghcjsWidgets :: !(GhcjsWidgets (GhcjsAppUrls -> FrontendWidgetT (R frontendRoute) ()))
    -- ^ Given the URLs of the compiled frontend's entry-point assets, return
    -- the widgets which are responsible for loading the script.
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

-- | URLs of the compiled frontend's entry-point assets, passed to the
-- 'GhcjsWidgets' in 'BackendConfig' so they can emit script and preload tags.
data GhcjsAppUrls = GhcjsAppUrls
  { _ghcjsAppUrls_allJs :: !Text
    -- ^ URL of @all.js@: the compiled GHCJS app, or the WASM bootstrap shim.
  , _ghcjsAppUrls_wasm :: !(Maybe Text)
    -- ^ URL of @frontend.wasm@ when the compiled frontend is a WASM build,
    -- detected by 'runBackendWith' from the frontend assets on disk.
  } deriving (Show, Eq, Ord, Generic)

-- | Given the URLs of the compiled frontend's entry-point assets, return the
-- widgets which are responsible for loading the script. Defaults to
-- 'preloadGhcjs' (plus 'preloadWasm' for WASM builds) and
-- 'deferredGhcjsScript'.
defaultGhcjsWidgets :: GhcjsWidgets (GhcjsAppUrls -> FrontendWidgetT r ())
defaultGhcjsWidgets = GhcjsWidgets
  { _ghcjsWidgets_preload = \urls -> do
      preloadGhcjs $ _ghcjsAppUrls_allJs urls
      mapM_ preloadWasm $ _ghcjsAppUrls_wasm urls
  , _ghcjsWidgets_script = deferredGhcjsScript . _ghcjsAppUrls_allJs
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

-- | URL at which the WASM frontend binary is served, mirroring 'renderAllJsPath'.
renderFrontendWasmPath :: Encoder Identity Identity (R (FullRoute a b)) PageName -> Text
renderFrontendWasmPath validFullEncoder =
  renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_Resource ResourceRoute_Ghcjs) :/ ["frontend.wasm"]

-- | Check whether the compiled frontend assets contain a @frontend.wasm@
-- binary, i.e. whether the frontend was built for the WASM target. Both the
-- unprocessed and the processed (content-addressed) asset layouts are probed.
frontendAssetsIncludeWasm :: StaticAssets -> IO Bool
frontendAssetsIncludeWasm assets = (||)
  <$> doesPathExist (_staticAssets_unprocessed assets </> "frontend.wasm")
  <*> doesPathExist (_staticAssets_processed assets </> "frontend.wasm")

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

data GhcjsAppRoute :: (Type -> Type) -> Type -> Type where
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
defaultBackendConfig = BackendConfig runSnapWithCommandLineArgs defaultStaticAssets defaultFrontendGhcjsAssets defaultGhcjsWidgets

-- | Run an obelisk backend with the default configuration.
runBackend :: Backend backendRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runBackend = runBackendWith defaultBackendConfig

-- | Run an obelisk backend with the given configuration.
runBackendWith
  :: BackendConfig frontendRoute
  -> Backend backendRoute frontendRoute
  -> Frontend (R frontendRoute)
  -> IO ()
runBackendWith (BackendConfig runSnap staticAssets frontendGhcjsAssets ghcjsWidgets) backend frontend = case checkEncoder $ _backend_routeEncoder backend of
  Left e -> fail $ "backend error:\n" <> T.unpack e
  Right validFullEncoder -> do
    publicConfigs <- getPublicConfigs
    hasWasm <- frontendAssetsIncludeWasm frontendGhcjsAssets
    let ghcjsAppUrls = GhcjsAppUrls
          { _ghcjsAppUrls_allJs = renderAllJsPath validFullEncoder
          , _ghcjsAppUrls_wasm = if hasWasm
              then Just $ renderFrontendWasmPath validFullEncoder
              else Nothing
          }
    _backend_run backend $ \serveRoute ->
      runSnap $
        getRouteWith validFullEncoder >>= \case
          Identity r -> case r of
            FullRoute_Backend backendRoute :/ a -> serveRoute $ backendRoute :/ a
            FullRoute_Frontend obeliskRoute :/ a ->
              serveObeliskApp routeToUrl (($ ghcjsAppUrls) <$> ghcjsWidgets) (serveStaticAssets staticAssets) frontendApp publicConfigs $
                obeliskRoute :/ a
              where
                routeToUrl (k :/ v) = renderObeliskRoute validFullEncoder $ FullRoute_Frontend (ObeliskRoute_App k) :/ v
                frontendApp = GhcjsApp
                  { _ghcjsApp_compiled = frontendGhcjsAssets
                  , _ghcjsApp_value = frontend
                  }

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

-- | Preload @frontend.wasm@ with a fetch hint matching the bootstrap shim's
-- request, so the (multi-megabyte) binary starts downloading before all.js
-- runs. Emitted by 'defaultGhcjsWidgets' when the frontend is a WASM build.
preloadWasm :: Text -> FrontendWidgetT r ()
preloadWasm wasmUrl = elAttr "link"
  (  "rel" =: "preload"
  <> "as" =: "fetch"
  <> "type" =: "application/wasm"
  <> "crossorigin" =: "anonymous"
  <> "href" =: wasmUrl
  ) blank

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

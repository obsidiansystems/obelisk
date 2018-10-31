{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Obelisk.Backend
  ( Backend (..)
  -- * Re-exports
  , Default (def)
  , getPageName
  , getRouteWith
  , runSnapWithCommandLineArgs
  , serveDefaultObeliskApp
  , prettifyOutput
  , runBackend
  , staticRenderContentType
  , mkRouteToUrl
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Categorical.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Obelisk.Asset.Serve.Snap (serveAsset)
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Snap.Extras (doNotCache, serveFileIfExistsAs)
import Reflex.Dom
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, modifyResponse
            , rqPathInfo, rqQueryString, setContentType, writeBS, writeText)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) Identity (R (Sum backendRoute (ObeliskRoute frontendRoute))) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  }

-- | The static assets provided must contain a compiled GHCJS app that corresponds exactly to the Frontend provided
data GhcjsApp route = GhcjsApp
  { _ghcjsApp_compiled :: !StaticAssets
  , _ghcjsApp_value :: !(Frontend route)
  }

-- | Serve a frontend, which must be the same frontend that Obelisk has built and placed in the default location
--TODO: The frontend should be provided together with the asset paths so that this isn't so easily breakable; that will probably make this function obsolete
serveDefaultObeliskApp :: MonadSnap m => (R appRoute -> Text) -> ([Text] -> m ()) -> Frontend (R appRoute) -> R (ObeliskRoute appRoute) -> m ()
serveDefaultObeliskApp urlEnc serveStaticAsset frontend = serveObeliskApp urlEnc serveStaticAsset frontendApp
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

runSnapWithCommandLineArgs :: Snap () -> IO ()
runSnapWithCommandLineArgs a = do
  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  httpServe httpConf a

getPageName :: (MonadSnap m) => m PageName
getPageName = do
  p <- getsRequest rqPathInfo
  q <- getsRequest rqQueryString
  let pageNameEncoder' :: Encoder Identity Identity PageName (String, String)
      pageNameEncoder' = bimap
        (unpackTextEncoder . pathSegmentsTextEncoder . listToNonEmptyEncoder)
        (unpackTextEncoder . queryParametersTextEncoder . toListMapEncoder)
  return $ decode pageNameEncoder' (T.unpack (decodeUtf8 p), T.unpack (decodeUtf8 q))

getRouteWith :: (MonadSnap m) => Encoder Identity parse route PageName -> m (parse route)
getRouteWith e = do
  pageName <- getPageName
  return $ tryDecode e pageName

serveObeliskApp :: MonadSnap m => (R appRoute -> Text) -> ([Text] -> m ()) -> GhcjsApp (R appRoute) -> R (ObeliskRoute appRoute) -> m ()
serveObeliskApp urlEnc serveStaticAsset frontendApp = \case
  ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> serveGhcjsApp urlEnc frontendApp $ GhcjsAppRoute_App appRouteComponent :/ appRouteRest
  ObeliskRoute_Resource resComponent :=> Identity resRest -> case resComponent :=> Identity resRest of
    ResourceRoute_Static :=> Identity pathSegments -> serveStaticAsset pathSegments
    ResourceRoute_Ghcjs :=> Identity pathSegments -> serveGhcjsApp urlEnc frontendApp $ GhcjsAppRoute_Resource :/ pathSegments
    ResourceRoute_JSaddleWarp :=> Identity _ -> do
      let msg = "Error: Obelisk.Backend received jsaddle request"
      liftIO $ putStrLn $ T.unpack msg
      writeText msg
    ResourceRoute_Version :=> Identity () -> doNotCache >> serveFileIfExistsAs "text/plain" "version"

serveStaticAssets :: MonadSnap m => StaticAssets -> [Text] -> m ()
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
serveGhcjsApp :: MonadSnap m => (R appRouteComponent -> Text) -> GhcjsApp (R appRouteComponent) -> R (GhcjsAppRoute appRouteComponent) -> m ()
serveGhcjsApp urlEnc app = \case
  GhcjsAppRoute_App appRouteComponent :=> Identity appRouteRest -> do
    modifyResponse $ setContentType staticRenderContentType
    writeBS <=< liftIO $ renderGhcjsFrontend urlEnc (appRouteComponent :/ appRouteRest) $ _ghcjsApp_value app
  GhcjsAppRoute_Resource :=> Identity pathSegments -> serveStaticAssets (_ghcjsApp_compiled app) pathSegments

runBackend :: Backend fullRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runBackend backend frontend = case checkEncoder $ _backend_routeEncoder backend of
  Left e -> fail $ "backend error:\n" <> T.unpack e
  Right validFullEncoder -> _backend_run backend $ \serveRoute -> do
    runSnapWithCommandLineArgs $ do
      getRouteWith validFullEncoder >>= \case
        Identity r -> case r of
          InL backendRoute :=> Identity a -> serveRoute $ backendRoute :/ a
          InR obeliskRoute :=> Identity a ->
            serveDefaultObeliskApp (mkRouteToUrl validFullEncoder) (serveStaticAssets defaultStaticAssets) frontend $ obeliskRoute :/ a

mkRouteToUrl :: Encoder Identity parse (R (Sum f (ObeliskRoute r))) PageName -> R r -> Text
mkRouteToUrl validFullEncoder =
  let pageNameEncoder' :: Encoder Identity (Either Text) PageName PathQuery = pageNameEncoder
  in \(k :/ v) -> T.pack . uncurry (<>) . encode pageNameEncoder' . encode validFullEncoder $ (InR $ ObeliskRoute_App k) :/ v


renderGhcjsFrontend :: MonadIO m => (route -> Text) -> route -> Frontend route -> m ByteString
renderGhcjsFrontend urlEnc route f = do
  let baseTag  = elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
      ghcjsPreload = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: "ghcjs/all.js") blank
      ghcjsScript = elAttr "script" ("language" =: "javascript" <> "src" =: "ghcjs/all.js" <> "defer" =: "defer") blank
  liftIO $ renderFrontendHtml urlEnc route
    (_frontend_head f >> injectExecutableConfigs >> baseTag >> ghcjsPreload)
    (_frontend_body f >> ghcjsScript)

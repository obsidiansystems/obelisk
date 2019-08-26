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
  , getPublicConfigs
  ) where

import Prelude hiding (id, (.))
import Control.Category
import Control.Monad
import Control.Monad.Except
import Control.Monad.Fail (MonadFail)
import Control.Categorical.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Obelisk.Asset.Serve.Snap (serveAsset)
import qualified Obelisk.ExecutableConfig.Lookup as Lookup
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Snap.Extras (doNotCache, serveFileIfExistsAs)
import Reflex.Dom
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, modifyResponse
            , rqPathInfo, rqQueryString, setContentType, writeBS, writeText
            , rqCookies, Cookie(..))
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
serveDefaultObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute
  -> Text)
  -> ([Text]
  -> m ())
  -> Frontend (R appRoute)
  -> Map Text ByteString
  -> R (ObeliskRoute appRoute)
  -> m ()
serveDefaultObeliskApp urlEnc serveStaticAsset frontend configs = serveObeliskApp urlEnc serveStaticAsset frontendApp configs
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

runSnapWithCommandLineArgs :: MonadIO m => Snap () -> m ()
runSnapWithCommandLineArgs a = do
  -- Get the web server configuration from the command line
  cmdLineConf <- liftIO $ commandLineConfig defaultConfig
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  liftIO $ httpServe httpConf a

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

serveObeliskApp
  :: (MonadSnap m, HasCookies m, MonadFail m)
  => (R appRoute -> Text)
  -> ([Text] -> m ())
  -> GhcjsApp (R appRoute)
  -> Map Text ByteString
  -> R (ObeliskRoute appRoute)
  -> m ()
serveObeliskApp urlEnc serveStaticAsset frontendApp config = \case
  ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> serveGhcjsApp urlEnc frontendApp config $ GhcjsAppRoute_App appRouteComponent :/ appRouteRest
  ObeliskRoute_Resource resComponent :=> Identity resRest -> case resComponent :=> Identity resRest of
    ResourceRoute_Static :=> Identity pathSegments -> serveStaticAsset pathSegments
    ResourceRoute_Ghcjs :=> Identity pathSegments -> serveGhcjsApp urlEnc frontendApp config $ GhcjsAppRoute_Resource :/ pathSegments
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
  -> GhcjsApp (R appRouteComponent)
  -> Map Text ByteString
  -> R (GhcjsAppRoute appRouteComponent)
  -> m ()
serveGhcjsApp urlEnc app config = \case
  GhcjsAppRoute_App appRouteComponent :=> Identity appRouteRest -> do
    modifyResponse $ setContentType staticRenderContentType
    writeBS <=< renderGhcjsFrontend urlEnc (appRouteComponent :/ appRouteRest) config $ _ghcjsApp_value app
  GhcjsAppRoute_Resource :=> Identity pathSegments -> serveStaticAssets (_ghcjsApp_compiled app) pathSegments

runBackend :: Backend fullRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runBackend backend frontend = case checkEncoder $ _backend_routeEncoder backend of
  Left e -> fail $ "backend error:\n" <> T.unpack e
  Right validFullEncoder -> do
    publicConfigs <- getPublicConfigs
    _backend_run backend $ \serveRoute -> do
      runSnapWithCommandLineArgs $ do
        getRouteWith validFullEncoder >>= \case
          Identity r -> case r of
            InL backendRoute :=> Identity a -> serveRoute $ backendRoute :/ a
            InR obeliskRoute :=> Identity a ->
              serveDefaultObeliskApp (mkRouteToUrl validFullEncoder) (serveStaticAssets defaultStaticAssets) frontend publicConfigs $
                obeliskRoute :/ a

mkRouteToUrl :: Encoder Identity parse (R (Sum f (ObeliskRoute r))) PageName -> R r -> Text
mkRouteToUrl validFullEncoder =
  let pageNameEncoder' :: Encoder Identity (Either Text) PageName PathQuery = pageNameEncoder
  in \(k :/ v) -> T.pack . uncurry (<>) . encode pageNameEncoder' . encode validFullEncoder $ (InR $ ObeliskRoute_App k) :/ v

renderGhcjsFrontend
  :: (MonadSnap m, HasCookies m)
  => (route -> Text)
  -> route
  -> Map Text ByteString
  -> Frontend route
  -> m ByteString
renderGhcjsFrontend urlEnc route configs f = do
  let ghcjsPreload = elAttr "link" ("rel" =: "preload" <> "as" =: "script" <> "href" =: "ghcjs/all.js") blank
      ghcjsScript = elAttr "script" ("language" =: "javascript" <> "src" =: "ghcjs/all.js" <> "defer" =: "defer") blank
  cookies <- askCookies
  renderFrontendHtml configs cookies urlEnc route f ghcjsPreload ghcjsScript

instance HasCookies Snap where
  askCookies = map (\c -> (cookieName c, cookieValue c)) <$> getsRequest rqCookies

-- | Get configs from the canonical "public" locations (i.e., locations that obelisk expects to make available
-- to frontend applications, and hence visible to end users).
getPublicConfigs :: IO (Map Text ByteString)
getPublicConfigs = Map.filterWithKey (\k _ -> isMemberOf k ["common", "frontend"]) <$> Lookup.getConfigs
  where
    isMemberOf k = any (`T.isPrefixOf` k)

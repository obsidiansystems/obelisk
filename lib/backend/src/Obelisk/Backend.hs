{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  ) where

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Obelisk.Asset.Serve.Snap (serveAsset)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, rqPathInfo,
             rqQueryString, writeBS, writeText)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

data Backend backendRoute frontendRoute = Backend
  { _backend_routeEncoder :: Encoder (Either Text) (Either Text) (R (Sum backendRoute (ObeliskRoute frontendRoute))) PageName
  , _backend_run :: ((R backendRoute -> Snap ()) -> IO ()) -> IO ()
  }

-- | The static assets provided must contain a compiled GHCJS app that corresponds exactly to the Frontend provided
data GhcjsApp route = GhcjsApp
  { _ghcjsApp_compiled :: !StaticAssets
  , _ghcjsApp_value :: !(Frontend route)
  }

{-
--TODO: Expose the encoder check phase
-- | Start an Obelisk backend
backend
  :: ( Universe (R route) --TODO: This seems wrong - should be Universe (Some route)
     , OrdTag route Identity
     , ShowTag route Identity
     , check ~ Either Text --TODO: Replace with MonadError Text check
     )
  => BackendConfig route
  -> check (IO ())
backend cfg = do
  getRequestRoute <- checkGetRequestRoute $ _backendConfig_routeEncoder cfg --TODO: Report error better
  return $ do
    prettifyOutput

    runSnapWithCommandLineArgs $ do
      getRequestRoute >>= \case
        Left e -> writeText e
        Right r -> serveDefaultObeliskApp (_backendConfig_frontend cfg) r
-}

-- | Serve a frontend, which must be the same frontend that Obelisk has built and placed in the default location
--TODO: The frontend should be provided together with the asset paths so that this isn't so easily breakable; that will probably make this function obsolete
serveDefaultObeliskApp :: MonadSnap m => Frontend (R appRoute) -> R (ObeliskRoute appRoute) -> m ()
serveDefaultObeliskApp frontend = serveObeliskApp defaultStaticAssets frontendApp
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

getPageName :: (MonadSnap m, MonadError Text parse) => m (parse PageName)
getPageName = do
  p <- getsRequest rqPathInfo
  q <- getsRequest rqQueryString
  return $ _validEncoder_decode pageNameValidEncoder
    ( "/" <> T.unpack (decodeUtf8 p)
    , "?" <> T.unpack (decodeUtf8 q)
    )

getRouteWith :: (MonadSnap m, MonadError Text parse) => ValidEncoder parse route PageName -> m (parse route)
getRouteWith e = do
  pageName <- getPageName
  return $ pageName >>= _validEncoder_decode e

serveObeliskApp :: MonadSnap m => StaticAssets -> GhcjsApp (R appRoute) -> R (ObeliskRoute appRoute) -> m ()
serveObeliskApp staticAssets frontendApp = \case
  ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> serveGhcjsApp frontendApp $ GhcjsAppRoute_App appRouteComponent :/ appRouteRest
  ObeliskRoute_Resource resComponent :=> Identity resRest -> case resComponent :=> Identity resRest of
    ResourceRoute_Static :=> Identity pathSegments -> serveStaticAssets staticAssets pathSegments
    ResourceRoute_Ghcjs :=> Identity pathSegments -> serveGhcjsApp frontendApp $ GhcjsAppRoute_Resource :/ pathSegments
    ResourceRoute_JSaddleWarp :=> Identity _ -> do
      let msg = "Error: Obelisk.Backend received jsaddle request"
      liftIO $ putStrLn $ T.unpack msg
      writeText msg

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

--TODO: Don't assume we're being served at "/"
serveGhcjsApp :: MonadSnap m => GhcjsApp (R appRouteComponent) -> R (GhcjsAppRoute appRouteComponent) -> m ()
serveGhcjsApp app = \case
  GhcjsAppRoute_App appRouteComponent :=> Identity appRouteRest -> do
    indexHtml <- liftIO $ fmap snd $ renderStatic $ fmap fst $ runEventWriterT $ flip runRoutedT (pure $ appRouteComponent :/ appRouteRest) $ blankLoader $ _frontend_head $ _ghcjsApp_value app
    --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
    writeBS $ "<!DOCTYPE html>\n" <> indexHtml
  GhcjsAppRoute_Resource :=> Identity pathSegments -> serveStaticAssets (_ghcjsApp_compiled app) pathSegments

blankLoader :: DomBuilder t m => m () -> m ()
blankLoader headHtml = el "html" $ do
  el "head" $ do
    elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
    headHtml
  el "body" $ do
    --TODO: Hash the all.js path
    elAttr "script" ("language" =: "javascript" <> "src" =: "ghcjs/all.js" <> "defer" =: "defer") blank

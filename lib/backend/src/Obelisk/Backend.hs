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
  , runBackend
  , configureFrontend
  ) where

import Control.Monad
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Sum
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding
import Obelisk.Asset.Serve.Snap (serveAsset)
import Obelisk.ExecutableConfig.Inject (injectPure)
import Obelisk.Frontend
import Obelisk.Route
import Snap (MonadSnap, Snap, commandLineConfig, defaultConfig, getsRequest, httpServe, rqPathInfo,
             rqQueryString, writeBS, writeText)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.Directory
import System.FilePath
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
  GhcjsAppRoute_App appRouteComponent :=> Identity appRouteRest ->
    writeBS <=< liftIO $ renderFrontendHtml (appRouteComponent :/ appRouteRest) <=< configureFrontend $ ghcjsFrontend $ _ghcjsApp_value app
  GhcjsAppRoute_Resource :=> Identity pathSegments -> serveStaticAssets (_ghcjsApp_compiled app) pathSegments

runBackend :: Backend fullRoute frontendRoute -> Frontend (R frontendRoute) -> IO ()
runBackend backend frontend = case checkEncoder $ _backend_routeEncoder backend of
  Left e -> fail $ "backend error:\n" <> T.unpack e
  Right validFullEncoder -> _backend_run backend $ \serveRoute -> do
    runSnapWithCommandLineArgs $ do
      getRouteWith validFullEncoder >>= \case
        Left e -> writeText e
        Right r -> case r of
          InL backendRoute :=> Identity a -> serveRoute $ backendRoute :/ a
          InR obeliskRoute :=> Identity a -> serveDefaultObeliskApp frontend $ obeliskRoute :/ a

configureFrontend :: Frontend a -> IO (Frontend a)
configureFrontend f = do
  cfgC <- getConfigs "config/common"
  cfgF <- getConfigs "config/frontend"
  return $ f { _frontend_head = _frontend_head f >> mapM_ (uncurry injectPure) (cfgC <> cfgF) }
  where
    getConfigs :: FilePath -> IO [(FilePath, Text)]
    getConfigs fp = do
      ps <- listDirectory fp
      fmap concat $ forM ps $ \p -> do
        let fullpath = fp </> p
        dir <- doesDirectoryExist p
        if dir
          then getConfigs fullpath
          else do
            v <- T.readFile fullpath
            return [(fullpath, v)]

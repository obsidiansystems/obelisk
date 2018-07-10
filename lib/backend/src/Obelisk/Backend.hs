{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Obelisk.Backend
  ( backend
  , BackendConfig (..)
  -- * Re-exports
  , Default (def)
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Universe
import Obelisk.Asset.Serve.Snap (serveAsset)
import Obelisk.Frontend
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom
import System.IO (hSetBuffering, stdout, stderr, BufferMode (..))
import Snap (MonadSnap, httpServe, defaultConfig, commandLineConfig, getsRequest, rqPathInfo, rqQueryString, writeText, writeBS)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig (route :: * -> *) = BackendConfig
  { _backendConfig_frontend :: !(Frontend (R route))
  , _backendConfig_routeEncoder :: !(Encoder (Either Text) (Either Text) (R (ObeliskRoute route)) PageName)
  }

instance route ~ IndexOnlyRoute => Default (BackendConfig route) where
  def = BackendConfig
    { _backendConfig_frontend = Frontend
      { _frontend_head = return ()
      , _frontend_body = return ()
      , _frontend_routeEncoder = obeliskRouteEncoder indexOnlyRouteComponentEncoder indexOnlyRouteRestEncoder . Encoder (pure $ prismValidEncoder $ rPrism _ObeliskRoute_App) --TODO: This is mostly redundant with the _backendConfig_routeEncoder
      , _frontend_title = \_ -> "Obelisk App"
      , _frontend_notFoundRoute = \_ -> IndexOnlyRoute :/ ()
      }
    , _backendConfig_routeEncoder = obeliskRouteEncoder indexOnlyRouteComponentEncoder indexOnlyRouteRestEncoder
    }

-- | The static assets provided must contain a compiled GHCJS app that corresponds exactly to the Frontend provided
data GhcjsApp route = GhcjsApp
  { _ghcjsApp_compiled :: !StaticAssets
  , _ghcjsApp_value :: !(Frontend route)
  }

--TODO: Expose the encoder check phase
-- | Start an Obelisk backend
backend
  :: ( Universe (R route) --TODO: This seems wrong - should be Universe (Some route)
     , OrdTag route Identity
     , ShowTag route Identity
     )
  => BackendConfig route
  -> IO ()
backend cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  let Right getRequestRoute = checkGetRequestRoute $ _backendConfig_routeEncoder cfg --TODO: Report error better
  -- Start the web server
  httpServe httpConf $ do
    let staticAssets = StaticAssets
          { _staticAssets_processed = "static.jsexe.assets"
          , _staticAssets_unprocessed = "static.jsexe"
          }
        frontendApp = GhcjsApp
          { _ghcjsApp_compiled = StaticAssets
            { _staticAssets_processed = "frontend.jsexe.assets"
            , _staticAssets_unprocessed = "frontend.jsexe"
            }
          , _ghcjsApp_value = _backendConfig_frontend cfg
          }
    getRequestRoute >>= \case
      Left e -> writeText e
      Right r -> serveObeliskRoute staticAssets frontendApp r

checkGetRequestRoute :: (MonadSnap m, Monad check, MonadError Text parse) => Encoder check parse route PageName -> check (m (parse route))
checkGetRequestRoute routeEncoder = do
  routeValidEncoder <- checkEncoder routeEncoder
  return $ do
    p <- getsRequest rqPathInfo
    q <- getsRequest rqQueryString
    return $ _validEncoder_decode (pageNameValidEncoder . routeValidEncoder)
      ( "/" <> T.unpack (decodeUtf8 p)
      , "?" <> T.unpack (decodeUtf8 q)
      )

serveObeliskRoute :: MonadSnap m => StaticAssets -> GhcjsApp (R appRoute) -> R (ObeliskRoute appRoute) -> m ()
serveObeliskRoute staticAssets frontendApp = \case
  ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> serveGhcjsApp frontendApp $ GhcjsAppRoute_App appRouteComponent :/ appRouteRest
  ObeliskRoute_Resource resComponent :=> Identity resRest -> case resComponent :=> Identity resRest of
    ResourceRoute_Static :=> Identity pathSegments -> serveStaticAssets staticAssets pathSegments
    ResourceRoute_Ghcjs :=> Identity pathSegments -> serveGhcjsApp frontendApp $ GhcjsAppRoute_Resource :/ pathSegments
    ResourceRoute_JSaddleWarp :=> Identity _ -> error "asdf" --TODO

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

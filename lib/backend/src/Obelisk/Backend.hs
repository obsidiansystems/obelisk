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
import Control.Monad.IO.Class
import Control.Monad.Except
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Functor.Identity
import Data.Monoid
import Data.Some
import qualified Data.Some as Some
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
import Snap (httpServe, defaultConfig, commandLineConfig, getsRequest, rqPathInfo, rqQueryString, writeText, writeBS)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig (route :: * -> *) = BackendConfig
  { _backendConfig_frontend :: Frontend (R route)
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
  let frontend = _backendConfig_frontend cfg
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  let Right routeEncoderValid = checkEncoder $ _backendConfig_routeEncoder cfg --TODO: Report error better
  -- Start the web server
  httpServe httpConf $ do
    p <- getsRequest rqPathInfo
    q <- getsRequest rqQueryString
    let parsed = _validEncoder_decode (pageNameValidEncoder . routeEncoderValid)
                 ( "/" <> T.unpack (decodeUtf8 p)
                 , "?" <> T.unpack (decodeUtf8 q)
                 )
    liftIO $ putStrLn $ "Got route: " <> show parsed
    case parsed of
      Left e -> writeText e
      Right r -> case r of
        ObeliskRoute_App appRouteComponent :=> Identity appRouteRest -> do
          indexHtml <- liftIO $ fmap snd $ renderStatic $ fmap fst $ runEventWriterT $ flip runRoutedT (pure $ appRouteComponent :/ appRouteRest) $ blankLoader $ _frontend_head frontend
          --TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
          writeBS $ "<!DOCTYPE html>\n" <> indexHtml
        ObeliskRoute_Resource ResourceRoute_Static :=> Identity pathSegments -> serveAsset "static.assets" "static" $ T.unpack $ T.intercalate "/" pathSegments
        ObeliskRoute_Resource ResourceRoute_Ghcjs :=> Identity pathSegments -> serveAsset "frontend.jsexe.assets" "frontend.jsexe" $ T.unpack $ T.intercalate "/" pathSegments
        ObeliskRoute_Resource ResourceRoute_JSaddleWarp :=> Identity _ -> error "asdf"

blankLoader :: DomBuilder t m => m () -> m ()
blankLoader headHtml = el "html" $ do
  el "head" $ do
    elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
    headHtml
  el "body" $ do
    --TODO: Hash the all.js path
    elAttr "script" ("language" =: "javascript" <> "src" =: "ghcjs/all.js" <> "defer" =: "defer") blank

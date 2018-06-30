{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Backend
  ( backend
  , BackendConfig (..)
  -- * Re-exports
  , Default (def)
  ) where

import Prelude hiding ((.))

import Control.Category
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BSC8
import Data.Default (Default (..))
import Data.Dependent.Sum
import Data.Functor.Identity
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Obelisk.Asset.Serve.Snap (serveAsset, serveAssets)
import Obelisk.Route
import Obelisk.Snap
import Reflex.Dom
import System.IO (hSetBuffering, stdout, stderr, BufferMode (..))
import Snap (httpServe, defaultConfig, commandLineConfig, route, getsRequest, rqPathInfo, rqQueryString, writeText, writeBS)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig = BackendConfig
  { _backendConfig_head :: StaticWidget () ()
  }

instance Default BackendConfig where
  def = BackendConfig (return ())

-- | Start an Obelisk backend
backend :: ShowTag appRoute Identity => Encoder (Either Text) (Either Text) (R (ObeliskRoute appRoute)) PageName -> BackendConfig -> IO ()
backend routeEncoder cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  headHtml <- fmap snd $ renderStatic $ _backendConfig_head cfg
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
      appCfg = def & appConfig_initialHead .~ headHtml
  let Right routeEncoderValid = checkEncoder routeEncoder --TODO: Report error better
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
        ObeliskRoute_App _ :/ _ -> do
          (_, index) <- liftIO $ renderStatic blankLoader --TODO: Render to a Builder instead; don't allow IO
          writeBS $ "<!DOCTYPE html>\n" <> index
        ObeliskRoute_Resource ResourceRoute_Static :=> Identity pathSegments -> serveAsset "static.assets" "static" $ T.unpack $ T.intercalate "/" pathSegments
        ObeliskRoute_Resource ResourceRoute_Ghcjs :=> Identity ghcjsRoute -> case ghcjsRoute of
          GhcjsRoute_AllJs :=> Identity () -> serveAsset "frontend.jsexe.assets" "frontend.jsexe" "all.js"
        ObeliskRoute_Resource ResourceRoute_JSaddleWarp :=> Identity _ -> error "asdf"
  -- Start the web server
  httpServe httpConf $ route
    [ ("", serveApp "" appCfg)
    , ("", serveAssets "frontend.jsexe.assets" "frontend.jsexe") --TODO: Can we prevent naming conflicts between frontend.jsexe and static?
    , ("static", serveAssets "static.assets" "static")
    ]

blankLoader :: DomBuilder t m => m ()
blankLoader = el "html" $ do
  el "head" $ do
    elAttr "base" ("href" =: "/") blank --TODO: Figure out the base URL from the routes
  el "body" $ do
    --TODO: Hash the all.js path
    elAttr "script" ("language" =: "javascript" <> "src" =: "ghcjs/all.js" <> "defer" =: "defer") blank

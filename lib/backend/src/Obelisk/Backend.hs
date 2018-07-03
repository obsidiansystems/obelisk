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
import Obelisk.Route
import Reflex.Dom
import System.IO (hSetBuffering, stdout, stderr, BufferMode (..))
import Snap (httpServe, defaultConfig, commandLineConfig, getsRequest, rqPathInfo, rqQueryString, writeText, writeBS)
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))

--TODO: Add a link to a large explanation of the idea of using 'def'
-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig (route :: * -> *) = BackendConfig
  { _backendConfig_head :: !(StaticWidget () ())
  , _backendConfig_routeEncoder :: !(Encoder (Either Text) (Either Text) (R (ObeliskRoute route)) PageName)
  }

data IndexOnly :: * -> * where
  IndexOnly :: IndexOnly ()

instance Universe (Some IndexOnly) where
  universe = [Some.This IndexOnly]

indexOnlyRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some IndexOnly) (Maybe Text)
indexOnlyRouteComponentEncoder = enum1Encoder $ \case
  IndexOnly -> Nothing

indexOnlyRouteRestEncoder :: (Applicative check, MonadError Text parse) => IndexOnly a -> Encoder check parse a PageName
indexOnlyRouteRestEncoder = \case
  IndexOnly -> Encoder $ pure $ endValidEncoder mempty

instance route ~ IndexOnly => Default (BackendConfig route) where
  def = BackendConfig
    { _backendConfig_head = return ()
    , _backendConfig_routeEncoder = obeliskRouteEncoder indexOnlyRouteComponentEncoder indexOnlyRouteRestEncoder
    }

-- | Start an Obelisk backend
backend :: ShowTag route Identity => BackendConfig route -> IO ()
backend cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  indexHtml <- fmap snd $ renderStatic $ blankLoader $ _backendConfig_head cfg
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
        ObeliskRoute_App _ :=> Identity _ -> do
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

deriveGShow ''IndexOnly
deriveGEq ''IndexOnly
deriveGCompare ''IndexOnly

instance ShowTag IndexOnly Identity where
  showTaggedPrec = \case
    IndexOnly -> showsPrec

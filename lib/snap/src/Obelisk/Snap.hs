{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Snap where

import Obelisk.Asset.Serve.Snap
import Obelisk.Snap.Extras

import Snap

import Control.Lens
import Data.ByteString (ByteString)
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import Lucid
import System.FilePath
import Control.Monad.IO.Class

-- Data type for web app configuration
data AppConfig m
   = AppConfig { _appConfig_initialBody :: m ByteString
               , _appConfig_initialHead :: ByteString
               , _appConfig_serveJsexe :: Bool
               , _appConfig_jsexe :: FilePath
               }

-- Default instance for app configuration
instance Monad m => Default (AppConfig m) where
  def = AppConfig { _appConfig_initialBody = return mempty
                  , _appConfig_initialHead = mempty
                  , _appConfig_serveJsexe = True
                  , _appConfig_jsexe = "frontend.jsexe"
                  }

{- | Takes a location, app, and an app configuration as arguements and
 routes the request to the correct handler. If "appConfig_serveJsexe"
 returns "True", additional js paths and asset paths will be appended to
 handler. Otherwise, it will return "404 Not Found" -}
serveAppAt :: MonadSnap m => ByteString -> FilePath -> AppConfig m -> m ()
serveAppAt loc app cfg = do
  route $ [ (loc, ifTop $ serveStaticIndex cfg)
          , (loc, serveAssets (app </> "static.assets") (app </> "static"))
          , (loc <> "/version", doNotCache >> serveFileIfExistsAs "text/plain" (app </> "version"))
          ]
       ++ if _appConfig_serveJsexe cfg
            then [(loc, serveAssets (app </> frontendJsAssetsPath cfg) (app </> frontendJsPath cfg))]
            else []
       ++ [ (loc, doNotCache >> error404) ]

-- Helper function used in 'serveAppAt', writes a 404 error message in ByteString
error404 :: MonadSnap m => m ()
error404 = do
  modifyResponse $ setResponseCode 404
  writeBS "404 Not Found"

-- Partially applied 'serveAppAt', specialized to the root directory
serveApp :: MonadSnap m => FilePath -> AppConfig m -> m ()
serveApp = serveAppAt ""

-- Helper function used in 'serveAppAt', returns a frontendJs/... filepath
frontendJsPath :: AppConfig m -> FilePath
frontendJsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs" </> jsexe

-- Helper function used in 'serveAppAt', returns a frontendJs.assets/... filepath
frontendJsAssetsPath :: AppConfig m -> FilePath
frontendJsAssetsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs.assets" </> jsexe

-- Helper funtion used in 'serveAppAt', writes a lazy bytestring to the body of Http response
serveStaticIndex :: MonadSnap m => AppConfig m -> m ()
serveStaticIndex cfg = do
  -- Decode, pack, and append asset target to file path
  appJsPath <- liftIO $ getAssetPath (frontendJsAssetsPath cfg) "/all.js"
  initialBody <- _appConfig_initialBody cfg
  -- Render and write html head and body to response
  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"] -- meta-data charset description
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"] -- meta-data viewport description
      toHtmlRaw $ _appConfig_initialHead cfg
    body_ $ do -- <body>
      toHtmlRaw initialBody -- HtmlT monad wrapper
      script_ [type_ "application/javascript", src_ (maybe "/all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)

{- | Takes an AppConfig monad and uses it to generate & write Lazy ByteString
 to the body of the https response. This one comes with hard coded values
 for the logo -}
serveIndex :: MonadSnap m => AppConfig m -> m ()
serveIndex cfg = do
  appJsPath <- liftIO $ getAssetPath (frontendJsAssetsPath cfg) "all.js"
  writeLBS $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    body_ $ do
      script_ [type_ "application/javascript", src_ (maybe "/all.js" T.pack appJsPath), defer_ "defer"] ("" :: String)

makeLenses ''AppConfig

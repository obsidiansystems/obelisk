{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Obelisk.Snap where

import Obelisk.Asset.Serve.Snap
import Obelisk.Snap.Extras

import Snap

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import Lucid
import Reflex.Dom
import System.FilePath

-- | Head and body content of the app's index page
data StaticContent static = StaticContent
  { _staticContent_head :: static
  , _staticContent_body :: static
  } deriving (Functor, Foldable, Traversable)

type ReflexStaticContent = StaticContent (StaticWidget () ())

-- | Render the given static content with 'renderStatic'.
renderStaticContent :: ReflexStaticContent -> IO (StaticContent ByteString)
renderStaticContent = traverse $ fmap snd . renderStatic

-- Data type for web app configuration
data AppConfig
   = AppConfig { _appConfig_initialBody :: ByteString
               , _appConfig_initialHead :: ByteString
               , _appConfig_serveJsexe :: Bool
               , _appConfig_jsexe :: FilePath
               }

-- Default instance for app configuration
instance Default AppConfig where
  def = AppConfig { _appConfig_initialBody = mempty
                  , _appConfig_initialHead = mempty
                  , _appConfig_serveJsexe = True
                  , _appConfig_jsexe = "frontend.jsexe"
                  }

{- | Takes a location, app, and an app configuration as arguements and
 routes the request to the correct handler. If "appConfig_serveJsexe"
 returns "True", additional js paths and asset paths will be appended to
 handler. Otherwise, it will return "404 Not Found" -}
serveAppAt :: MonadSnap m => ByteString -> FilePath -> ReflexStaticContent -> AppConfig -> m ()
serveAppAt loc app static cfg = do
  route $ [ (loc, ifTop $ serveStaticIndex static cfg)
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
serveApp :: MonadSnap m => FilePath -> ReflexStaticContent -> AppConfig -> m ()
serveApp = serveAppAt ""

-- Helper function used in 'serveAppAt', returns a frontendJs/... filepath
frontendJsPath :: AppConfig -> FilePath
frontendJsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs" </> jsexe

-- Helper function used in 'serveAppAt', returns a frontendJs.assets/... filepath
frontendJsAssetsPath :: AppConfig -> FilePath
frontendJsAssetsPath (AppConfig { _appConfig_jsexe = jsexe }) = "frontendJs.assets" </> jsexe

-- Helper funtion used in 'serveAppAt', writes a lazy bytestring to the body of Http response
serveStaticIndex :: MonadSnap m => ReflexStaticContent -> AppConfig -> m ()
serveStaticIndex static cfg = do
  -- Decode, pack, and append asset target to file path
  appJsPath <- liftIO $ getAssetPath (frontendJsAssetsPath cfg) "/all.js"
  -- Render and write html head and body to response
  writeLBS =<< mkIndexLBS (maybe "/all.js" T.pack appJsPath) static

-- | Render the index page with the given js file and static content
mkIndexLBS
  :: MonadIO m
  => T.Text -> ReflexStaticContent -> m LBS.ByteString
mkIndexLBS js static = liftIO $ do
  rendered <- renderStaticContent static
  pure $ renderBS $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"] -- meta-data charset description
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"] -- meta-data viewport description
      toHtmlRaw $ _staticContent_head rendered
    body_ $ do -- <body>
      toHtmlRaw $ _staticContent_body rendered
      script_ [type_ "application/javascript", src_ js, defer_ "defer"] ("" :: String)


makeLenses ''AppConfig

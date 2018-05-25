{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Obelisk.Backend
  ( runObeliskBackend
  , mkIndexLBS
  , StaticContent(..)
  , ReflexStaticContent
  , renderStaticContent
  , BackendConfig (..)
  -- * Re-exports
  , Default (def)
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Monoid
import qualified Data.Text as T
import Lucid
import Obelisk.Asset.Serve.Snap
import Obelisk.Snap.Extras
import Reflex.Dom
import Snap
import Snap.Internal.Http.Server.Config (Config (accessLog, errorLog), ConfigLog (ConfigIoLog))
import System.IO (hSetBuffering, stderr, BufferMode (..))
import System.FilePath

-- | Head and body content of the app's index page
data StaticContent static = StaticContent
  { _staticContent_head :: static
  , _staticContent_body :: static
  } deriving (Functor, Foldable, Traversable)

type ReflexStaticContent = StaticContent (StaticWidget () ())

-- | Render the given static content with 'renderStatic'.
renderStaticContent :: ReflexStaticContent -> IO (StaticContent ByteString)
renderStaticContent = traverse (fmap snd . renderStatic)

-- | Configure the operation of the Obelisk backend.  For reasonable defaults,
-- use 'def'.
data BackendConfig = BackendConfig
  { _backendConfig_serveJsexe :: Bool
  , _backendConfig_jsexe :: FilePath
  }

instance Default BackendConfig where
  def = BackendConfig
    { _backendConfig_serveJsexe = True
    , _backendConfig_jsexe = "frontend.jsexe"
    }

-- | Start an Obelisk backend
runObeliskBackend
  :: ReflexStaticContent -- ^ The static content
  -> BackendConfig -- ^ Other configuration
  -> IO ()
runObeliskBackend static cfg = do
  -- Make output more legible by decreasing the likelihood of output from
  -- multiple threads being interleaved
  hSetBuffering stderr LineBuffering

  -- Get the web server configuration from the command line
  cmdLineConf <- commandLineConfig defaultConfig
  let httpConf = cmdLineConf
        { accessLog = Just $ ConfigIoLog BSC8.putStrLn
        , errorLog = Just $ ConfigIoLog BSC8.putStrLn
        }
  -- Start the web server
  httpServe httpConf $ serveApp "" static cfg

{- | Takes a location, app, and an app configuration as arguements and
 routes the request to the correct handler. If "backendConfig_serveJsexe"
 returns "True", additional js paths and asset paths will be appended to
 handler. Otherwise, it will return "404 Not Found" -}
serveAppAt
  :: MonadSnap m
  => ByteString -> FilePath -> ReflexStaticContent -> BackendConfig
  -> m ()
serveAppAt loc app static cfg = route $
  [ (loc, ifTop $ serveStaticIndex static cfg)
  , (loc, serveAssets (app </> "static.assets") (app </> "static"))
  , (loc <> "/version", doNotCache >> serveFileIfExistsAs "text/plain" (app </> "version"))
  ] ++ if _backendConfig_serveJsexe cfg
       then [(loc, serveAssets (app </> frontendJsAssetsPath cfg) (app </> frontendJsPath cfg))]
       else []
    ++ [ (loc, doNotCache >> error404) ]

-- | Helper function used in 'serveAppAt', writes a 404 error message in ByteString
error404 :: MonadSnap m => m ()
error404 = do
  modifyResponse $ setResponseCode 404
  writeBS "404 Not Found"

-- | Partially applied 'serveAppAt', specialized to the root directory
serveApp :: MonadSnap m => FilePath -> ReflexStaticContent -> BackendConfig -> m ()
serveApp = serveAppAt ""

-- | Helper function used in 'serveAppAt', returns a frontendJs/... filepath
frontendJsPath :: BackendConfig -> FilePath
frontendJsPath (BackendConfig { _backendConfig_jsexe = jsexe }) = "frontendJs" </> jsexe

-- | Helper function used in 'serveAppAt', returns a frontendJs.assets/... filepath
frontendJsAssetsPath :: BackendConfig -> FilePath
frontendJsAssetsPath (BackendConfig { _backendConfig_jsexe = jsexe }) = "frontendJs.assets" </> jsexe

-- | Helper funtion used in 'serveAppAt', writes a lazy bytestring to the body of Http response
serveStaticIndex :: MonadSnap m => ReflexStaticContent -> BackendConfig -> m ()
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

makeLenses ''BackendConfig

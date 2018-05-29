{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig.Core where

import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadCatch, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.FilePath
import Text.URI (ParseException, URI, mkURI)
import Text.URI.Lens

-- Config: common/route
newtype CommonRoute = CommonRoute URI
  deriving (Eq, Show, Ord, Generic)

instance FromJSON CommonRoute where
  parseJSON = undefined -- XXX

class ObeliskConfigOld a where
  readObeliskConfig :: (MonadIO m, MonadCatch m) => FilePath -> m (Either String a)

instance ObeliskConfigOld CommonRoute where
  readObeliskConfig p = fmap (validateRoute <=< bimap toErrorString CommonRoute) $ try $ liftIO $
    mkURI =<< T.readFile routeFile
    where
      routeFile = p </> "config" </> "common" </> "route"
      toErrorString = displayException :: ParseException -> String
      validateRoute uri = maybe (Left "No port") (const $ Right uri) $ getRoutePort uri

getRoutePort :: CommonRoute -> Maybe Word
getRoutePort (CommonRoute uri) = uri ^? uriAuthority . _Right . authPort . _Just

-- new design

-- | Path to a specific config file in the project.
data ConfigPath config
  = ConfigPath_Common FilePath
  | ConfigPath_Frontend FilePath
  | ConfigPath_Backend FilePath
  deriving (Eq, Show, Ord)

-- | Relative filepath to the config file.
--
-- Relative to the project root.
getConfigPath :: ConfigPath config -> FilePath
getConfigPath = \case
  ConfigPath_Common fp -> "config" </> "common" </> fp
  ConfigPath_Backend fp -> "config" </> "backend" </> fp
  ConfigPath_Frontend fp -> "config" </> "frontend" </> fp

class ObeliskConfig a where
  configPath :: ConfigPath a
  parseConfig :: BLS.ByteString -> Maybe a

instance ObeliskConfig CommonRoute where
  configPath = ConfigPath_Common "route"
  parseConfig = fmap CommonRoute . mkURI . T.decodeUtf8 . BLS.toStrict

-- Sample config the user may add
data EmailConfig = EmailConfig
  { _emailConfig_hostname :: Text
  , _emailConfig_port :: Int
  }
  deriving (Eq, Show, Generic)

instance FromJSON EmailConfig

instance ObeliskConfig EmailConfig where
  -- Indicate that this is a backend config.
  -- Config file path will be: config/backend/email-server
  configPath = ConfigPath_Backend "email-server"
  -- Treat this config as a JSON file.
  parseConfig = decode

getConfig
  :: (MonadIO m, FromJSON config)
  => FilePath
  -> ConfigPath config
  -> m (Maybe config)
getConfig root g = liftIO $ BLS.readFile path >>= return . decode
  where
    path = root </> getConfigPath g

getRouteConfig :: MonadIO m => FilePath -> m (Maybe CommonRoute)
getRouteConfig root = getConfig root $ ConfigPath_Common "route"

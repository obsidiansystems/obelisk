{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
-- | TODO: Consider renaming 'Core' to 'Types'
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
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.FilePath
import Text.URI (ParseException, URI, mkURI)
import Text.URI.Lens

-- Config: common/route
newtype Route = Route URI
  deriving (Eq, Show, Ord, Generic)

class ObeliskConfigOld a where
  readObeliskConfig :: (MonadIO m, MonadCatch m) => FilePath -> m (Either String a)

instance ObeliskConfigOld Route where
  readObeliskConfig p = fmap (validateRoute <=< bimap toErrorString Route) $ try $ liftIO $
    mkURI =<< T.readFile routeFile
    where
      routeFile = p </> "config" </> "common" </> "route"
      toErrorString = displayException :: ParseException -> String
      validateRoute uri = maybe (Left "No port") (const $ Right uri) $ getRoutePort uri

getRoutePort :: Route -> Maybe Word
getRoutePort (Route uri) = uri ^? uriAuthority . _Right . authPort . _Just

-- Prototype

data CabalProject
  = CabalProject_Backend
  | CabalProject_Common
  | CabalProject_Frontend
  deriving (Eq, Show, Ord)

cabalProjectName :: IsString a => CabalProject -> a
cabalProjectName = \case
  CabalProject_Backend -> "backend"
  CabalProject_Common -> "common"
  CabalProject_Frontend -> "frontend"

-- | Path to a specific config file in the project.
data ConfigPath config = ConfigPath CabalProject FilePath
  deriving (Eq, Show, Ord)

-- | Relative filepath to the config file.
--
-- Relative to the project root.
getConfigPath :: ConfigPath config -> FilePath
getConfigPath (ConfigPath cProj fp)= "config" </> cabalProjectName cProj </> fp

class ObeliskConfig a where
  configPath :: ConfigPath a
  parseConfig :: BLS.ByteString -> Maybe a

instance ObeliskConfig Route where
  configPath = ConfigPath CabalProject_Common "route"
  parseConfig = fmap Route . mkURI . T.decodeUtf8 . BLS.toStrict

getConfig
  :: forall m config. (MonadIO m, ObeliskConfig config)
  => FilePath
  -> m (Maybe config)
getConfig root = liftIO $ BLS.readFile p >>= return . parseConfig
  where
    p = getConfigPath (configPath :: ConfigPath config)

getRouteConfig :: MonadIO m => FilePath -> m (Maybe Route)
getRouteConfig root = getConfig root

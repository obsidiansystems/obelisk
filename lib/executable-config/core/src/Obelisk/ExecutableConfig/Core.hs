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
import Control.Monad.Catch (Exception, MonadCatch, MonadThrow, catch, displayException, throwM, try)
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
import Data.Typeable
import GHC.Generics
import System.FilePath
import Text.URI (ParseException, URI, mkURI)
import Text.URI.Lens

-- Config: common/route
newtype Route = Route URI
  deriving (Eq, Show, Ord, Generic)

data MissingPort = MissingPort
  deriving (Eq, Show, Typeable)

instance Exception MissingPort

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
  parseConfig :: MonadThrow m => BLS.ByteString -> m a

instance ObeliskConfig Route where
  configPath = ConfigPath CabalProject_Common "route"
  parseConfig c = do
    uri <- mkURI (T.decodeUtf8 $ BLS.toStrict c)
    Route <$> validate uri
    where
      validate uri
        = maybe (throwM MissingPort) (const $ pure uri) $
          getRoutePort (Route uri)

getConfig'
  :: forall m config. (MonadIO m, MonadThrow m, ObeliskConfig config)
  => FilePath
  -> m config
getConfig' root = liftIO $ BLS.readFile p >>= parseConfig
  where
    p = getConfigPath (configPath :: ConfigPath config)

getConfig
  :: forall m e config.
     (MonadIO m, Exception e, MonadCatch m, ObeliskConfig config)
  => FilePath
  -> m (Either e config)
getConfig root = try (getConfig' root)

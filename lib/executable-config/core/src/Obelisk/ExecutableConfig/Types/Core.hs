{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig.Types.Core where

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
import System.FilePath

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

-- | An Obelisk config file item
class ObeliskConfig a where
  configPath :: ConfigPath a
  parseConfig :: MonadThrow m => BLS.ByteString -> m a
  configToText :: a -> Text

-- | Retrieve the specified config
--
-- Throws exceptions during retrieval if any.
getConfig'
  :: forall m config. (MonadIO m, MonadThrow m, ObeliskConfig config)
  => FilePath
  -> m config
getConfig' root = liftIO $ BLS.readFile p >>= parseConfig
  where
    p = getConfigPath (configPath :: ConfigPath config)

-- | Retrieve the specified config
--
-- Returns Either wrapping the underlying exception.
getConfig
  :: forall m e config.
     (MonadIO m, Exception e, MonadCatch m, ObeliskConfig config)
  => FilePath
  -> m (Either e config)
getConfig root = try (getConfig' root)

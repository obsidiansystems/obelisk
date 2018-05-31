{-# LANGUAGE AllowAmbiguousTypes #-}
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
data ConfigLocation config = ConfigLocation CabalProject FilePath
  deriving (Eq, Show, Ord)

-- | An Obelisk config file item
class ObeliskConfig a where
  -- | Location of this config in the project
  configLocation :: ConfigLocation a
  -- | How to decode the config from a bytestring
  decodeConfig :: MonadThrow m => BLS.ByteString -> m a
  -- | How to encode the config back to text
  encodeConfig :: a -> Text

-- | Get the relative path to the config file
getConfigPath :: forall config. ObeliskConfig config => FilePath
getConfigPath = f $ configLocation @config
  where
    f (ConfigLocation p l) = "config" </> cabalProjectName p </> l

-- | Retrieve the specified config
--
-- Throws exceptions during retrieval if any.
getConfig'
  :: forall m config. (MonadIO m, MonadThrow m, ObeliskConfig config)
  => FilePath
  -> m config
getConfig' root = liftIO $ BLS.readFile p >>= decodeConfig
  where
    p = getConfigPath @config

-- | Retrieve the specified config
--
-- Returns Either wrapping the underlying exception.
getConfig
  :: forall m e config. (MonadIO m, Exception e, MonadCatch m, ObeliskConfig config)
  => FilePath
  -> m (Either e config)
getConfig root = try $ getConfig' root

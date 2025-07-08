{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class(MonadIO, liftIO)

import Obelisk.Configs.Internal.Directory (getConfigsFromDirectory)

getConfigs :: MonadIO m => m (Map Text ByteString)
getConfigs = liftIO $ getConfigsFromDirectory "config"

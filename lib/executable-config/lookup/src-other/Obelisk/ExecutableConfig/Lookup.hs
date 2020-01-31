{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Obelisk.Configs.Internal.Directory (getConfigsFromDirectory)

getConfigs :: IO (Map Text ByteString)
getConfigs = getConfigsFromDirectory "config"

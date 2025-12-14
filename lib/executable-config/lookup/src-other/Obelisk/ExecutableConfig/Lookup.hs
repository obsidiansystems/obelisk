{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup
  ( getConfigs
  , escapeVarName
  , EnvCodec(..)
  , unescapeVarName
  )
where

import Data.Map (Map)
import Data.Text (Text)
import Data.ByteString (ByteString)

import Obelisk.Configs.Internal.Directory (getConfigsFromDirectory)

import Obelisk.Configs.Internal.Environment

getConfigs :: IO (Map Text ByteString)
getConfigs = getConfigsFromDirectory "config" >>= getConfigsFromEnvironment "OBELISK_CONFIG_"

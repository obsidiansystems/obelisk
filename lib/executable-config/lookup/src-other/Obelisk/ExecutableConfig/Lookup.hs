{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup
  ( getConfigs
  , escapeVarName
  , EnvCodec(..)
  , unescapeVarName
  )
where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Control.Monad

import Obelisk.Configs.Internal.Directory

import Obelisk.Configs.Internal.Environment

getConfigs :: IO (Map Text ByteString)
getConfigs = getConfigsFromDirectory "config" >>= getConfigsFromEnvironment "OBELISK_CONFIG_"

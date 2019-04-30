module Obelisk.ExecutableConfig.Lookup where

import Data.Map (Map)
import Data.Text (Text)

import Obelisk.ExecutableConfig.Internal.ConfigDirectory

getConfigs :: IO (Map Text Text)
getConfigs = getConfigsFromDirectory "." "config"

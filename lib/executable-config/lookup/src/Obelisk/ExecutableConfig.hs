module Obelisk.ExecutableConfig where

import Data.Map (Map)
import Data.Text (Text)

import Obelisk.ExecutableConfig.Internal.ConfigDirectory

getFrontendConfigs :: IO (Map Text Text)
getFrontendConfigs = do
  cfgC <- getConfigs "." "config/common"
  cfgF <- getConfigs "." "config/frontend"
  return $ cfgC <> cfgF

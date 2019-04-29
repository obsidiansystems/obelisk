module Obelisk.ExecutableConfig (get) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Obelisk.ExecutableConfig.Internal
import Obelisk.ExecutableConfig.Internal.AssetManager

get :: Text -> IO (Maybe Text)
get name = bracket getAssets freeAssetManager $ \mgrObj -> do
  mgr <- assetManagerFromJava mgrObj
  getFromMgr mgr $ T.encodeUtf8 name

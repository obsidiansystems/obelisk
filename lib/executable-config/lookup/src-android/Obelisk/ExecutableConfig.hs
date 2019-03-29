module Obelisk.ExecutableConfig (get) where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (withCString)

import Obelisk.ExecutableConfig.Internal
import Obelisk.ExecutableConfig.Internal.AssetManager

get :: Text -> IO (Maybe Text)
get name = bracket getAssets freeAssetManager $ \mgrObj -> do
  mgr <- assetManagerFromJava mgrObj
  getFromMgr mgr (withCString . T.unpack) name

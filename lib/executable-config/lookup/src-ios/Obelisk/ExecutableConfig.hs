module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView
import System.FilePath.Posix ((</>))
import System.IO.Error

import Obelisk.ExecutableConfig.Types

-- XXX: Needs testing
get :: forall config. ObeliskConfig config => IO config
get = fmap join $ mainBundleResourcePath >>= \mp -> forM mp $ \p -> do
  let root = T.unpack (T.decodeUtf8 p)
      path = getConfigPath configPath
  BLS.readFile (root </> path) >>= parseConfig

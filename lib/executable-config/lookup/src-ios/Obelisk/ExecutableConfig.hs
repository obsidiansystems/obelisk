module Obelisk.ExecutableConfig (get) where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView
import System.FilePath.Posix ((</>))

import Obelisk.ExecutableConfig.Internal (catchDoesNotExist)

get :: Text -> IO (Maybe Text)
get name = fmap join $ mainBundleResourcePath >>= \mp -> forM mp $ \p -> 
  catchDoesNotExist  $ T.readFile $ T.unpack (T.decodeUtf8 p) </> "config" </> T.unpack name

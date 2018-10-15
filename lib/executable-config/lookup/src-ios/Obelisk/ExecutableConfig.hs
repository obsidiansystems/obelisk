module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView
import System.FilePath.Posix ((</>))
import System.IO.Error

get :: Text -> IO (Maybe Text)
get name = fmap join $ mainBundleResourcePath >>= \mp -> forM mp $ \p -> 
  catchDoesNotExist  $ T.readFile $ T.unpack (T.decodeUtf8 p) </> T.unpack name

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist f = catchJust doesNotExist (Just <$> f) $ const $ return Nothing
  where
    doesNotExist e = if isDoesNotExistError e then Just () else Nothing

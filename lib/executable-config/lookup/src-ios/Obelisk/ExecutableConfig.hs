{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as BLS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.Javascript.JSaddle.WKWebView
import System.FilePath.Posix ((</>))
import System.IO.Error

import Obelisk.ExecutableConfig.Types

get :: forall config. ObeliskConfig config => IO config
get = mainBundleResourcePath >>= \(Just p) -> do -- FIXME: Handle Nothing
  let root = T.unpack $ T.decodeUtf8 p
      path = getConfigPath (configLocation :: ConfigLocation config)
  BLS.readFile (root </> path) >>= decodeConfig

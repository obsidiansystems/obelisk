{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
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

data NoBundleResource = NoBundleResource
  deriving Show

instance Exception NoBundleResource

get :: forall config. ObeliskConfig config => IO config
get = mainBundleResourcePath >>= \case
  Nothing -> throw NoBundleResource
  Just p -> do
    let root = T.unpack $ T.decodeUtf8 p
        path = getConfigPath @config
    BLS.readFile (root </> path) >>= decodeConfig

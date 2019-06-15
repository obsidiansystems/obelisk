{-# LANGUAGE LambdaCase #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView
import System.FilePath.Posix

import Obelisk.Configs.Internal.Directory

getConfigs :: IO (Map Text ByteString)
getConfigs = mainBundleResourcePath >>= \case
  Nothing -> error "Could not get bundle resource path."
  Just p -> getConfigsFromDirectory $ T.unpack (T.decodeUtf8 p) </> "config"

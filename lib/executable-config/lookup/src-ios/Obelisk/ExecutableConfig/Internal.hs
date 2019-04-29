{-# LANGUAGE LambdaCase #-}
module Obelisk.ExecutableConfig.Internal where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView

import Obelisk.ExecutableConfig.Internal.ConfigDirectory

getFrontendConfigs :: IO (Map Text Text)
getFrontendConfigs = mainBundleResourcePath >>= \case
  Nothing -> error "Could not get bundle resource path."
  Just p -> getConfigs (T.unpack (T.decodeUtf8 p)) "config"

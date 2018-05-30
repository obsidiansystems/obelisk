{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.ExecutableConfig where

import qualified Data.ByteString.Char8 as BLC
import qualified Data.ByteString.Lazy as BLS
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.NonElementParentNode

import Obelisk.ExecutableConfig.Types

get :: forall config. ObeliskConfig config => IO config
get = do
  let path = getConfigPath (configPath :: ConfigPath config)
  -- FIXME: Handle Nothing values and them throwM
  Just doc <- currentDocument
  Just e <- getElementById doc $ "injected-" <> path
  content :: Text <- getInnerHTML e
  parseConfig $ BLS.fromStrict $ BLC.pack $ T.unpack content

{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig where

import Control.Monad.Trans.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import GHCJS.DOM
import GHCJS.DOM.Element (getInnerHTML)
import GHCJS.DOM.NonElementParentNode

get :: Text -> IO (Maybe Text)
get key = runMaybeT $ do
  doc <- MaybeT currentDocument
  e <- MaybeT $ getElementById doc $ "config-" <> key
  getInnerHTML e

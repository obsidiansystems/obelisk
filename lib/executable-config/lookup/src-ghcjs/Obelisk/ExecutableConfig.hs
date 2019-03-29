{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig where

import Control.Monad.Trans.Maybe
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import GHCJS.DOM
import GHCJS.DOM.Document (getHead)
import GHCJS.DOM.Element (Element, getElementsByTagName, getId, getInnerHTML)
import GHCJS.DOM.HTMLCollection (HTMLCollection, item, getLength)
import GHCJS.DOM.NonElementParentNode

get :: Text -> IO (Maybe Text)
get item = runMaybeT $ do
  doc <- MaybeT currentDocument
  e <- MaybeT $ getElementById doc $ "config-" <> item
  getInnerHTML e

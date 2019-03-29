{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Internal where

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

getFrontendConfigs :: IO [(Text, Text)]
getFrontendConfigs = fmap concat $ runMaybeT $ do
  doc <- MaybeT currentDocument
  hd <- MaybeT $ getHead doc
  es <- collToList =<< getElementsByTagName hd ("script" :: Text)
  cfg <- for es $ \e -> runMaybeT $ do
    ident <- getId e
    k <- MaybeT $ pure $ T.stripPrefix "config-" ident
    v <- getInnerHTML e
    pure (k,v)
  pure $ sortOn fst $ catMaybes cfg
  where
    collToList es = do
      len <- getLength es
      list <- traverse (item es) [0..len-1]
      pure $ catMaybes list

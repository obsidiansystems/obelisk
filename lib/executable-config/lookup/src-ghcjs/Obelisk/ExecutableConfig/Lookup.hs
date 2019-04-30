{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import GHCJS.DOM
import GHCJS.DOM.Document (getHead)
import GHCJS.DOM.Element (getElementsByClassName, getId, getInnerHTML)
import GHCJS.DOM.HTMLCollection (item, getLength)

getConfigs :: IO (Map Text Text)
getConfigs = do
  Just doc <- currentDocument
  Just hd <- getHead doc
  es <- collToList =<< getElementsByClassName hd ("obelisk-executable-config-inject" :: Text)
  cfg <- for es $ \e -> do
    ident <- getId e
    v <- getInnerHTML e
    return $ case T.stripPrefix "config-" ident of
      Just k -> (k, v)
      Nothing -> error $
        "Element '" <> show ident <> "' has class 'obelisk-executable-config-inject' but does not start with 'config-'."
  return $ Map.fromList cfg
  where
    collToList es = do
      len <- getLength es
      list <- traverse (item es) [0..len-1]
      pure $ catMaybes list

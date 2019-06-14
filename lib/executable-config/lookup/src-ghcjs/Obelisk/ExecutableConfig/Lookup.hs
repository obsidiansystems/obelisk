{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import GHCJS.DOM
import GHCJS.DOM.Document (getHead)
import GHCJS.DOM.DOMStringMap (get)
import GHCJS.DOM.Element (getInnerHTML)
import GHCJS.DOM.HTMLElement (HTMLElement(HTMLElement), getDataset)
import GHCJS.DOM.NodeList (item, getLength)
import GHCJS.DOM.ParentNode (querySelectorAll)
import GHCJS.DOM.Types (Node(Node), castTo)

getConfigs :: IO (Map Text ByteString)
getConfigs = do
  Just doc <- currentDocument
  Just hd <- getHead doc
  nodes <- collToList =<< querySelectorAll hd ("[data-obelisk-executable-config-inject-key]" :: Text)
  fmap Map.fromList $ for nodes $ \node -> do
    e <- castTo HTMLElement node >>= \case
      Nothing -> error "Found node with data attribute obelisk-executable-config-inject-key that is not an HTMLElement."
      Just htmlE -> return htmlE
    dataset <- getDataset e
    (,)
      <$> get dataset ("obelisk-executable-config-inject-key" :: Text)
      <*> (fmap decodeOrFail (getInnerHTML e))
  where
    collToList es = do
      len <- getLength es
      list <- traverse (item es) [0..len-1]
      pure $ catMaybes list
    decodeOrFail x = case B64.decode (T.decodeUtf8 x) of
      Left e -> error ("Obelisk.ExecutableConfig.Lookup.getConfigs: error when decoding base64: " ++ e)
      Right x -> x
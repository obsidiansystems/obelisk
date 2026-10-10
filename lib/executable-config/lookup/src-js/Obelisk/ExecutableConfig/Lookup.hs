module Obelisk.ExecutableConfig.Lookup where

import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import GHCJS.DOM
import GHCJS.DOM.DOMStringMap (get)
import GHCJS.DOM.Document (getHead)
import GHCJS.DOM.Element (getInnerHTML)
import GHCJS.DOM.HTMLElement (HTMLElement (HTMLElement), getDataset)
import GHCJS.DOM.NodeList (IsNodeList, getLength, item)
import GHCJS.DOM.ParentNode (querySelectorAll)
import GHCJS.DOM.Types (MonadJSM, Node (Node), castTo)
import Language.Javascript.JSaddle (JSM)

getConfigs :: JSM (Map Text ByteString)
getConfigs = do
  Just doc <- currentDocument
  Just hd <- getHead doc
  nodes <- nodeListNodes =<< querySelectorAll hd ("[data-obelisk-executable-config-inject-key]" :: Text)
  fmap Map.fromList $ for nodes $ \node -> do
    e <-
      castTo HTMLElement node >>= \case
        Nothing -> error "Found node with data attribute obelisk-executable-config-inject-key that is not an HTMLElement."
        Just htmlE -> pure htmlE
    dataset <- getDataset e
    (,)
      -- the key is camelCased: https://html.spec.whatwg.org/multipage/dom.html#dom-dataset
      <$> get dataset ("obeliskExecutableConfigInjectKey" :: Text)
      <*> fmap decodeOrFail (getInnerHTML e)
  where
    decodeOrFail x = case B64.decode (T.encodeUtf8 x) of
      Left e -> error ("Obelisk.ExecutableConfig.Lookup.getConfigs: error when decoding base64: " <> e)
      Right x' -> x'

-- | Collect all nodes in the node list.
--
-- TODO: this and the version in obelisk-frontend should be
-- upstreamed to jsaddle.
nodeListNodes :: (IsNodeList l, MonadJSM m) => l -> m [Node]
nodeListNodes es = do
  len <- getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (item es) $ if len == 0 then [] else [0 .. len - 1]
  pure $ catMaybes nodes

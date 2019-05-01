{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Inject where

import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import Data.Text (Text)
import Reflex.Dom.Core hiding (value)

-- | Produces a @<script>@ tag with an @id@ attribute containing the key and
-- whose value is the provided configuration
injectPure :: DomBuilder t m => Text -> Text -> m ()
injectPure key value =
  let attrs = ("type" =: "text/plain" <> "data-obelisk-executable-config-inject-key" =: key)
  in elAttr "script" attrs $ text value

-- | Produces injectable @<script>@ tags containing the configuration keys
-- (filepaths) and values.
injectExecutableConfigs :: (MonadIO m, DomBuilder t m) => Map Text Text -> m ()
injectExecutableConfigs = mapM_ (uncurry injectPure) . Map.toList

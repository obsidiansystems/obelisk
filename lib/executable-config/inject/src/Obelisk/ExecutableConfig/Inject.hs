{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.ExecutableConfig.Inject where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Control.Monad (mapM_)
import Data.Semigroup ((<>))
#endif
#endif
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Reflex.Dom.Core hiding (value)

-- | Produces a @<script>@ tag with an @id@ attribute containing the key and
-- whose value is the provided configuration, encoded in base64.
injectPure :: DomBuilder t m => Text -> ByteString -> m ()
injectPure key value =
  let attrs = ("type" =: "text/plain" <> "data-obelisk-executable-config-inject-key" =: key <> "data-hydration-skip" =: "")
   in elAttr "script" attrs $ text (T.decodeUtf8 (B64.encode value))

-- | Produces injectable @<script>@ tags containing the configuration keys
-- (filepaths) and values.
injectExecutableConfigs :: (MonadIO m, DomBuilder t m) => Map Text ByteString -> m ()
injectExecutableConfigs = mapM_ (uncurry injectPure) . Map.toList

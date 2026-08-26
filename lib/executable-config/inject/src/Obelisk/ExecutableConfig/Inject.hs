module Obelisk.ExecutableConfig.Inject where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Control.Monad (mapM_)
import Data.Semigroup ((<>))
#endif
#endif
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Reflex.Dom.Core hiding (value)

-- | Produces a @<script>@ tag with an @id@ attribute containing the key and
-- whose value is the provided configuration, encoded in base64.
injectPure :: DomBuilder t m => Text -> ByteString -> m ()
injectPure key value =
  let attrs = ("type" =: "text/plain" <> "data-obelisk-executable-config-inject-key" =: key <> "data-hydration-skip" =: "")
  in elAttr "script" attrs $ text (T.decodeUtf8Lenient (B64.encode value))

-- | Produces injectable @<script>@ tags containing the configuration keys
-- (filepaths) and values.
injectExecutableConfigs :: (MonadIO m, DomBuilder t m) => Map Text ByteString -> m ()
injectExecutableConfigs = mapM_ (uncurry injectPure) . Map.toList

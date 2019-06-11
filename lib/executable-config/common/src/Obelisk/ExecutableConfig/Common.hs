{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Obelisk.ExecutableConfig.Common
  ( HasCommonConfigs(..)
  ) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Reflex
import Reflex.Dom.Core

class Monad m => HasCommonConfigs m where
  getCommonConfig :: Text -> m (Maybe Text)
  default getCommonConfig :: (HasCommonConfigs m', m ~ t m', MonadTrans t) => Text -> m (Maybe Text)
  getCommonConfig = lift . getCommonConfig

instance HasCommonConfigs m => HasCommonConfigs (BehaviorWriterT t w m)
instance HasCommonConfigs m => HasCommonConfigs (DynamicWriterT t w m)
instance HasCommonConfigs m => HasCommonConfigs (EventWriterT t w m)
instance HasCommonConfigs m => HasCommonConfigs (PostBuildT t m)
instance HasCommonConfigs m => HasCommonConfigs (QueryT t q m)
instance HasCommonConfigs m => HasCommonConfigs (ReaderT r m)
instance HasCommonConfigs m => HasCommonConfigs (RequesterT t request response m)
instance HasCommonConfigs m => HasCommonConfigs (StaticDomBuilderT t m)
instance HasCommonConfigs m => HasCommonConfigs (TriggerEventT t m)

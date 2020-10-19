{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Configs
  ( HasConfigs(..)
  , ConfigsT
  , runConfigsT
  , mapConfigsT
  , getTextConfig
  ) where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Fail (MonadFail)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Morph (MFunctor)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.Ref (MonadRef)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader (ReaderT (..), ask, mapReaderT)
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.Trans.State.Strict as Strict
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Reflex
import Reflex.Host.Class (MonadReflexCreateTrigger)
import Reflex.Dom.Core
  ( DomBuilder
  , DomRenderHook
  , HasDocument
  , HasJS
  , HasJSContext
  , Prerender (Client)
  , StaticDomBuilderT
  , prerender
  )
#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle (MonadJSM)
#endif

class Monad m => HasConfigs m where
  getConfigs :: m (Map Text ByteString)
  default getConfigs :: (HasConfigs m', m ~ t m', MonadTrans t) => m (Map Text ByteString)
  getConfigs = lift getConfigs
  getConfig :: Text -> m (Maybe ByteString)
  getConfig k = do
    configs <- getConfigs
    return $ Map.lookup k configs

instance Monad m => HasConfigs (ConfigsT m) where
  getConfigs = ConfigsT ask

getTextConfig :: HasConfigs m => Text -> m (Maybe Text)
getTextConfig k = fmap T.decodeUtf8 <$> getConfig k

instance HasConfigs m => HasConfigs (BehaviorWriterT t w m)
instance HasConfigs m => HasConfigs (DynamicWriterT t w m)
instance HasConfigs m => HasConfigs (EventWriterT t w m)
instance HasConfigs m => HasConfigs (PostBuildT t m)
instance HasConfigs m => HasConfigs (QueryT t q m)
instance HasConfigs m => HasConfigs (ReaderT r m)
instance HasConfigs m => HasConfigs (RequesterT t request response m)
instance HasConfigs m => HasConfigs (StateT w m)
instance HasConfigs m => HasConfigs (Strict.StateT w m)
instance HasConfigs m => HasConfigs (StaticDomBuilderT t m)
instance HasConfigs m => HasConfigs (TriggerEventT t m)

newtype ConfigsT m a = ConfigsT { unConfigsT :: ReaderT (Map Text ByteString) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadPlus
    , Alternative
    , MonadFail
    , MonadFix
    , MonadThrow
    , MonadIO
    , MonadBase m'
    , MonadBaseControl m'
    , MonadRef
    , MonadTrans
    , MFunctor
    , DomBuilder t
    , MonadHold t
    , MonadReflexCreateTrigger t
    , MonadSample t
    , NotReady t
    , PostBuild t
    , TriggerEvent t
    , HasDocument
    , DomRenderHook t
    , HasJSContext
    , HasJS js
#ifndef ghcjs_HOST_OS
    , MonadJSM
#endif
    )

instance PerformEvent t m => PerformEvent t (ConfigsT m) where
  type Performable (ConfigsT m) = ConfigsT (Performable m)
  performEvent e = ConfigsT $ ReaderT $ \configs ->
    performEvent $ runConfigsT configs <$> e
  performEvent_ e = ConfigsT $ ReaderT $ \configs ->
    performEvent_ $ runConfigsT configs <$> e

instance Adjustable t m => Adjustable t (ConfigsT m) where
  runWithReplace a e = ConfigsT $ runWithReplace (unConfigsT a) (unConfigsT <$> e)
  traverseDMapWithKeyWithAdjust f m e = ConfigsT $ traverseDMapWithKeyWithAdjust (\k v -> unConfigsT $ f k v) m e
  traverseIntMapWithKeyWithAdjust f m e = ConfigsT $ traverseIntMapWithKeyWithAdjust (\k v -> unConfigsT $ f k v) m e
  traverseDMapWithKeyWithAdjustWithMove f m e = ConfigsT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unConfigsT $ f k v) m e

instance Prerender js t m => Prerender js t (ConfigsT m) where
  type Client (ConfigsT m) = ConfigsT (Client m)
  prerender server client = ConfigsT $ ReaderT $ \configs ->
    prerender (runConfigsT configs server) (runConfigsT configs client)

instance PrimMonad m => PrimMonad (ConfigsT m) where
  type PrimState (ConfigsT m) = PrimState m
  primitive = lift . primitive

runConfigsT
  :: Map Text ByteString
  -> ConfigsT m a
  -> m a
runConfigsT cs child = runReaderT (unConfigsT child) cs

mapConfigsT
  :: (forall x. m x -> n x)
  -> ConfigsT m a
  -> ConfigsT n a
mapConfigsT f (ConfigsT x) = ConfigsT $ mapReaderT f x

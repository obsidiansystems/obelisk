{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig.Inject where

import Control.Exception (Exception, throw)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex.Dom
import System.FilePath ((</>))

import Obelisk.ExecutableConfig.Types

data InjectionDisallowed = forall config. ObeliskConfig config
  => InjectionDisallowed (ConfigLocation config)

deriving instance Show InjectionDisallowed
instance Exception InjectionDisallowed

inject :: forall config. ObeliskConfig config => config -> IO ByteString
inject cfg = do
  let loc = configLocation @config
  unless (canInject loc) $
    throw $ InjectionDisallowed loc
  fmap snd $ renderStatic $
    elAttr "script" ("type" =: "text/plain" <> "id" =: key) $
      text $ encodeConfig cfg
  where
    key = "injected-" <> T.pack (getConfigPath @config)

-- | Check if we are allowed to inject the given config into the DOM.
-- TODO: Can we perform this check at the compile time?
canInject :: ObeliskConfig config => ConfigLocation config -> Bool
canInject = \case
  ConfigLocation CabalProject_Backend _ -> False
  _ -> True

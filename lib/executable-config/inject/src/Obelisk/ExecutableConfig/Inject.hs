{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.ExecutableConfig.Inject where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex.Dom
import System.FilePath ((</>))

import Obelisk.ExecutableConfig.Types

inject :: forall config. ObeliskConfig config => config -> IO ByteString
inject cfg = do
  fmap snd $ renderStatic $
    elAttr "script" ("type" =: "text/plain" <> "id" =: key) $
      text $ configToText cfg
  where
    key = case configPath :: ConfigPath config of
      ConfigPath CabalProject_Backend _ ->
        error "not allowed" -- XXX: check this compile time
      cfgPath ->
        "injected-" <> T.pack (getConfigPath cfgPath)

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
      text $ encodeConfig cfg
  where
    key = case configLocation :: ConfigLocation config of
      ConfigLocation CabalProject_Backend _ ->
        error "not allowed" -- XXX: check this compile time
      _ ->
        "injected-" <> T.pack $ getConfigPath @config

{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Inject where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex.Dom
import System.FilePath ((</>))

inject :: FilePath -> IO ByteString
inject item = do
  f <- BS.readFile $ "exe-config" </> item
  fmap snd $ renderStatic $
    elAttr "script" ("type" =: "text/plain" <> "id" =: ("config-" <> T.pack item)) $
      text $ T.decodeUtf8 f

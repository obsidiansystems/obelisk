{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Inject where

import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reflex.Dom.Core
import System.FilePath ((</>))

inject :: FilePath -> IO ByteString
inject item = do
  f <- T.readFile $ "config" </> item
  fmap snd $ renderStatic $ injectPure item f

injectPure :: DomBuilder t m => FilePath -> T.Text -> m ()
injectPure item content = elAttr "script" ("type" =: "text/plain" <> "id" =: ("config-" <> T.pack item)) $
  text content

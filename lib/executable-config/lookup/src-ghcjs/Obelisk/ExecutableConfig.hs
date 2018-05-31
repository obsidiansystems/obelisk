{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig where

import Control.Exception (Exception, throw)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import qualified Data.ByteString.Char8 as BLC
import qualified Data.ByteString.Lazy as BLS
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.NonElementParentNode

import Obelisk.ExecutableConfig.Types

data NoInjectedContent = NoInjectedContent
  deriving Show

instance Exception NoInjectedContent

get :: forall config. ObeliskConfig config => IO config
get = do
  let path = getConfigPath @config
  mContent <- runMaybeT $ do
    doc <- MaybeT currentDocument
    e <- MaybeT $ getElementById doc $ "injected-" <> path
    getInnerHTML e
  case mContent of
    Nothing -> throw NoInjectedContent
    Just content ->
      decodeConfig $ BLS.fromStrict $ BLC.pack $ T.unpack content

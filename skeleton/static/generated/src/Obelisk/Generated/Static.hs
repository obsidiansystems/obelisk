{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Generated.Static where

import qualified GHC.Types
import Data.Text (Text)
import qualified Data.Text.Internal
import Data.Monoid ((<>))

static :: forall a. StaticFile a => Text
static = "static/" <> staticPath @a

class StaticFile (s :: GHC.Types.Symbol)
    where {staticPath :: Data.Text.Internal.Text}
instance StaticFile "css/.keep"
    where {staticPath = "css/0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73-.keep"}
instance StaticFile "html/.keep"
    where {staticPath = "html/0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73-.keep"}
instance StaticFile "icons/.keep"
    where {staticPath = "icons/0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73-.keep"}
instance StaticFile "images/.keep"
    where {staticPath = "images/0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73-.keep"}
instance StaticFile "js/.keep"
    where {staticPath = "js/0mdqa9w1p6cmli6976v4wi0sw9r4p5prkj7lzfd1877wk11c9c73-.keep"}

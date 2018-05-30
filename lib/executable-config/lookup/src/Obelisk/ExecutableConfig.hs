{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.FilePath.Posix ((</>))
import System.IO.Error

import Obelisk.ExecutableConfig.Types

get :: forall config. ObeliskConfig config => IO config
get = getConfig' ""

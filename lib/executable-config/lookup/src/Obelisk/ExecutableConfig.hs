{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig (get) where

import Control.Exception
import qualified Data.ByteString.Lazy as BLS
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.FilePath.Posix ((</>))
import System.IO.Error

import Obelisk.ExecutableConfig.Types

get :: forall config. ObeliskConfig config => IO config
get = BLS.readFile (getConfigPath @config) >>= decodeConfig

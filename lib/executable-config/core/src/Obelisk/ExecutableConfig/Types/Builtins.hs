{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.ExecutableConfig.Types.Builtins where

import Control.Monad.Catch (Exception, throwM)
import qualified Data.ByteString.Lazy as BLS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Typeable
import GHC.Generics

import Control.Lens
import Text.URI (ParseException, URI, mkURI)
import Text.URI.Lens

import Obelisk.ExecutableConfig.Types.Core (CabalProject (..), ConfigLocation (..), ObeliskConfig (..))

-- Config: common/route
newtype Route = Route URI
  deriving (Eq, Show, Ord, Generic)

data MissingPort = MissingPort
  deriving (Eq, Show, Typeable)

instance Exception MissingPort

getRoutePort :: Route -> Maybe Word
getRoutePort (Route uri) = uri ^? uriAuthority . _Right . authPort . _Just

instance ObeliskConfig Route where
  configLocation = ConfigLocation CabalProject_Common "route"
  decodeConfig c = do
    uri <- mkURI (T.decodeUtf8 $ BLS.toStrict c)
    Route <$> validate uri
    where
      validate uri =
        maybe (throwM MissingPort) (const $ pure uri) $
        getRoutePort (Route uri)
  encodeConfig = T.pack . show

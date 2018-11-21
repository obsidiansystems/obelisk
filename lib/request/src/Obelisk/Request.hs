{-# LANGUAGE GADTs #-}

module Obelisk.Request
  ( Request (..)
  , SomeRequest (..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, parseJSON, toJSON)
import Data.Aeson.Types (Parser)
import Data.Constraint (Dict)

class Request r where
  requestToJSON :: r a -> Value
  requestParseJSON :: Value -> Parser (SomeRequest r)
  requestResponseToJSON :: r a -> Dict (ToJSON a)
  requestResponseFromJSON :: r a -> Dict (FromJSON a)

data SomeRequest t where
    SomeRequest :: (FromJSON x, ToJSON x) => t x -> SomeRequest t

instance Request r => FromJSON (SomeRequest r) where
  parseJSON = requestParseJSON

instance Request r => ToJSON (SomeRequest r) where
  toJSON (SomeRequest r) = requestToJSON r

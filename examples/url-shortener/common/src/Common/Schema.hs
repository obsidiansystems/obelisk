{-# LANGUAGE DeriveGeneric #-}
module Common.Schema where

import Data.Text (Text)
import Database.Id.Class
import GHC.Generics

newtype Url = Url { unUrl :: Text }
  deriving Generic

instance HasId Url

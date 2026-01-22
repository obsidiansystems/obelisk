{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Common.Route.Checked where

import Data.Functor.Identity
import qualified Data.Text as Text
import Obelisk.Route

import Common.Route

case fullRouteEncoder of
  Left err -> error $ Text.unpack err
  Right _ ->
    [d|
      validFullEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
      Right validFullEncoder = fullRouteEncoder
    |]

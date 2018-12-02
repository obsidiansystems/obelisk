{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Obelisk.Request
  ( Request
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Constraint.Extras
import Data.Some (Some)

type Request r = (FromJSON (Some r), ToJSON (Some r), Has FromJSON r, Has ToJSON r) --TODO: shouldn't Has FromJSON r imply FromJSON (Some r) ?

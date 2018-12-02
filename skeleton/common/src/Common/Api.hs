{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Api where

import Common.Schema
import Data.Aeson
import Data.Aeson.GADT
import Data.AppendMap ()
import Data.Constraint.Extras.TH
import Data.Functor.Identity
import Data.Map.Monoidal (MonoidalMap (..))
import qualified Data.Map.Monoidal as Map
import Data.Vessel
import Data.Semigroup
import Data.Text (Text)
import Data.Functor.Compose
import Reflex.Class
import Reflex.Query.Class

type VS = MapV
-- ()

type V = MapV
-- PatchMapWithMove2 (Id Task) (TaskT Maybe)

type family Id t where
  Id (f Identity) = PrimaryKey f Identity

data MyRequest a where
  MyRequest_Add :: Text -> MyRequest (PrimaryKey TaskT Identity)

--TODO: deriveArgDict REQUIRES PolyKinds; see if we can check for that in TH
deriveArgDict ''MyRequest
deriveJSONGADT ''MyRequest

instance (Ord k, Ord k', Semigroup v) => Query (MapV (k :: *) (v :: *) (Compose (MonoidalMap k') (Const SelectedCount))) where
  type QueryResult (MapV k v (Compose (MonoidalMap k') (Const SelectedCount))) = MapV k v (Compose (MonoidalMap k') Identity)

instance (Ord k, Semigroup v) => Query (MapV (k :: *) (v :: *) (Const SelectedCount)) where
  type QueryResult (MapV k v (Const SelectedCount)) = MapV k v Identity

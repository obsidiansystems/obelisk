{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Api where

import Common.Schema
import Data.Aeson
import Data.AppendMap ()
import Data.Functor.Identity
import Data.Map.Monoidal (MonoidalMap (..))
import qualified Data.Map.Monoidal as Map
import Data.Semigroup
import Data.Text (Text)
import Obelisk.Request.TH
import Reflex.Class
import Reflex.Query.Class

newtype VS k v a = VS (MonoidalMap k a)
  deriving (Semigroup, Monoid, Functor, Foldable, Traversable, Group, ToJSON, FromJSON, Eq)

newtype V k v a = V (MonoidalMap k (a, First (Maybe v)))
  deriving (Semigroup, Monoid, Functor, Foldable, Traversable, ToJSON, FromJSON, Eq)

instance FunctorMaybe (V k v) where
  fmapMaybe f (V m) = V $ Map.mapMaybe (\(a, v) -> (,v) <$> f a) m

instance (Ord k, Semigroup a) => Query (VS k v a) where
  type QueryResult (VS k v a) = V k v a
  crop _ = id

type family Id t where
  Id (f Identity) = PrimaryKey f Identity

data MyRequest a where
  MyRequest_Add :: Text -> MyRequest (Id Task)

makeRequestForData ''MyRequest

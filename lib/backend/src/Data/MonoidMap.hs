{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.MonoidMap where

import Data.Align
import Data.Map.Monoidal (MonoidalMap)
import Data.Map.Monoidal as Map
import Data.Semigroup (Semigroup, (<>))
import Reflex (Query, QueryResult, crop, FunctorMaybe (..))

newtype MonoidMap k v = MonoidMap { unMonoidMap :: MonoidalMap k v }
  deriving (Show, Eq, Ord, Foldable)

monoidMap :: (Eq v, Monoid v) => MonoidalMap k v -> MonoidMap k v
monoidMap = MonoidMap . Map.filter (/= mempty)

instance (Eq (QueryResult q), Ord k, Query q) => Query (MonoidMap k q) where
  type QueryResult (MonoidMap k q) = MonoidMap k (QueryResult q)
  crop (MonoidMap q) (MonoidMap qr) =
    -- This assumes that the query result of a null query should be null
    monoidMap $ Map.intersectionWith crop q qr

instance (Monoid a, Eq a, Ord k) => Semigroup (MonoidMap k a) where
  MonoidMap a <> MonoidMap b =
    let combine _ a' b' =
          let c = a' `mappend` b'
          in if c == mempty
               then Nothing
               else Just c
    in MonoidMap $ Map.mergeWithKey combine id id a b

instance (Ord k, Monoid a, Eq a) => Monoid (MonoidMap k a) where
  mempty = MonoidMap Map.empty
  mappend = (<>)

instance (Ord k) => Functor (MonoidMap k) where
  fmap f (MonoidMap x) = MonoidMap (fmap f x)

instance FunctorMaybe (MonoidMap k) where
  fmapMaybe f (MonoidMap m) = MonoidMap $ Map.mapMaybe f m

instance Ord k => Align (MonoidMap k) where
  nil = MonoidMap $ MonoidalMap nil
  align (MonoidMap (MonoidalMap x)) (MonoidMap (MonoidalMap y)) = MonoidMap $ MonoidalMap $ align x y
  alignWith f (MonoidMap (MonoidalMap x)) (MonoidMap (MonoidalMap y)) = MonoidMap $ MonoidalMap $ alignWith f x y

instance Ord k => Unalign (MonoidMap k)

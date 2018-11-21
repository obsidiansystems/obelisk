{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Obelisk.Request.HList (HList (..)) where

import Data.Aeson (FromJSON, ToJSON, Value (Array), parseJSON, toJSON, withArray)
import Data.Vector (Vector)
import qualified Data.Vector as V

data family HList (l::[*])

data instance HList '[] = HNil
data instance HList (x ': xs) = x `HCons` HList xs

infixr 2 `HCons`

deriving instance Eq (HList '[])
deriving instance (Eq x, Eq (HList xs)) => Eq (HList (x ': xs))

deriving instance Ord (HList '[])
deriving instance (Ord x, Ord (HList xs)) => Ord (HList (x ': xs))

deriving instance Bounded (HList '[])
deriving instance (Bounded x, Bounded (HList xs)) => Bounded (HList (x ': xs))

instance (FromJSON h, FromJSON (HList t)) => FromJSON (HList (h ': t)) where
  parseJSON = withArray "HList (a ': t)" $ \v -> do
    Just (aVal, v') <- return $ vectorView v
    a <- parseJSON aVal
    b <- parseJSON $ Array v'
    return $ HCons a b

instance FromJSON (HList '[]) where
  parseJSON = withArray "HList '[]" $ \v -> do
    Nothing <- return $ vectorView v
    return HNil

class HListToJSON l where
  hListToJSON :: HList l -> [Value]

instance (ToJSON h, HListToJSON t) => HListToJSON (h ': t) where
  hListToJSON (HCons h t) = toJSON h : hListToJSON t

instance HListToJSON '[] where
  hListToJSON HNil = []

instance HListToJSON l => ToJSON (HList l) where
  toJSON = Array . V.fromList . hListToJSON

vectorView :: Vector a -> Maybe (a, Vector a)
vectorView v =
  if V.length v > 0
  then Just (V.head v, V.tail v)
  else Nothing

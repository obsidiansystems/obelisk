{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.Tabulation where

import Control.Lens

-- | This is a class for record types whose fields can be enumerated by an associated GADT. It's closely related to the concept of a representable functor, except without the functor part, and the fields are not all the same type.
class HasFields a where
  type Field a :: * -> *

  fieldLens :: Field a x -> Lens' a x
  tabulateFieldsA :: Applicative f => (forall x. Field a x -> f x) -> f a

  tabulateFields :: (forall x. Field a x -> x) -> a
  tabulateFields f = runIdentity (tabulateFieldsA (Identity . f))
  traverseWithField :: Applicative m => (forall x. Field a x -> x -> m x) -> a -> m a
  traverseWithField t r = tabulateFieldsA (\f -> t f (indexField r f))
  indexField :: a -> Field a x -> x
  indexField a f = a ^. fieldLens f
  {-# MINIMAL fieldLens, tabulateFieldsA #-}
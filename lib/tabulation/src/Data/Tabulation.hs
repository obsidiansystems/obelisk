{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

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

-- | A wrapper to allow mixing/reusing a non-HKD associated GADT with the higher kinded data record.

{- |
==== __Usage example__
We can do

@
data XY = XY
  { _x :: ()
  , _y :: 'Bool'
  }

data XYField a where
  XYField_X :: XYField ()
  XYField_Y :: XYField 'Bool'

instance HasFields XY where
  type Field XY = XYField
  fieldLens = \\case
    XYField_X -> \\f (XY x y) -> flip XY y '<$>' f x
    XYField_Y -> lens _y $ \\xy y -> xy { _y = y }
  tabulateFieldsA g = pure XY
    '<*>' g XYField_X
    '<*>' g XYField_Y
@

so one would expect being able to

@
data XYHKD f = XYHKD
  { _x' :: f ()
  , _y' :: f Bool
  }

data XYHKDField a where
  XYHKDField_X :: XYHKDField (f ())
  XYHKDField_Y :: XYHKDField (f Bool)

instance HasFields (XYHKD f) where
  type Field (XYHKD f) = XYHKDField
  fieldLens = \\case
    XYHKDField_X -> \\f (XYHKD x y) -> flip XYHKD y '<$>' f x
    XYHKDField_Y -> lens _y' $ \\xy y -> xy { _y' = y }
  tabulateFieldsA g = pure XYHKD
    '<*>' g XYHKDField_X
    '<*>' g XYHKDField_Y
@

While `tabulateFieldsA` compiles, `fieldLens` doesn't, with the same sort of error in both its cases.

@
   • Could not deduce: f2 ~ f
      from the context: x ~ f2 ()
        bound by a pattern with constructor:
                   XYHKDField_X :: forall (f :: * -> *). XYHKDField (f ()),
                 in a case alternative
        at ../lib/tabulation/src/Data/Tabulation.hs:68:5-16
      ‘f2’ is a rigid type variable bound by
        a pattern with constructor:
          XYHKDField_X :: forall (f :: * -> *). XYHKDField (f ()),
        in a case alternative
        at ../lib/tabulation/src/Data/Tabulation.hs:68:5-16
      ‘f’ is a rigid type variable bound by
        the instance declaration
        at ../lib/tabulation/src/Data/Tabulation.hs:65:10-28
      Expected type: x
        Actual type: f ()
@

The `XYHKDField` constructors must work for all `f`, but the one from `XYHKD` is a specific one (it's quantified outside), so this direction doesn't work, but the one in `tabulateFieldsA` does.

We can fix this by threading the `f` from the record to the associated GADT.

@
data XYHKDField f a where
  XYHKDField_X :: XYHKDField f (f ())
  XYHKDField_Y :: XYHKDField f (f Bool)

instance HasFields (XYHKD f) where
  type Field (XYHKD f) = XYHKDField f
@

but that's cumbersome and takes some work to figure out once you first try to use `Field` with HKD.

To solve these problems, one can use `FieldHKD` instead.

@
instance HasFields (XYHKD f) where
  type Field (XYHKD f) = FieldHKD XYField f
  fieldLens = \\case
    FieldHKD XYField_X -> \\f (XYHKD x y) -> flip XYHKD y '<$>' f x
    FieldHKD XYField_Y -> lens _y' $ \\xy y -> xy { _y' = y }
  tabulateFieldsA g = pure XYHKD
    '<*>' g (FieldHKD XYField_X)
    '<*>' g (FieldHKD XYField_Y)
@

This also lets us capture the notion that the fields of the HKD version are related to the base version by a `f` layer by transforming one into the other.

@
liftKind
  :: ('HasFields' t, 'HasFields' (t' f), 'Field' (t' f) ~ FieldHKD ('Field' t) f)
  => (forall x. x -> f x) -> t -> t' f
liftKind f r = 'tabulateFields' $ \(FieldHKD field) -> f ('indexField' r field)

unliftKind
  :: ('HasFields' t, 'HasFields' (t' f), 'Field' (t' f) ~ FieldHKD ('Field' t) f)
  => (forall x. f x -> x) -> t' f -> t
unliftKind f r = 'tabulateFields' $ \field -> f ('indexField' r (FieldHKD field))

xyPure :: 'Applicative' f => XY -> XYHKD f
xyPure = 'liftKind' 'pure'

xyRunIdentity :: XYHKD 'Identity' -> XY
xyRunIdentity = 'unliftKind' 'runIdentity'
@
-}
data FieldHKD field f x where
  FieldHKD :: field x -> FieldHKD field f (f x)

liftKind
  :: (HasFields t, HasFields (t' f), Field (t' f) ~ FieldHKD (Field t) f)
  => (forall x. x -> f x) -> t -> t' f
liftKind f r = tabulateFields $ \(FieldHKD field) -> f (indexField r field)

unliftKind
  :: (HasFields t, HasFields (t' f), Field (t' f) ~ FieldHKD (Field t) f)
  => (forall x. f x -> x) -> t' f -> t
unliftKind f r = tabulateFields $ \field -> f (indexField r (FieldHKD field))

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Prelude hiding (id, (.))

import Control.Applicative (liftA2)
import Control.Categorical.Bifunctor (bimap)
import Control.Category (Category((.), id))
import Control.Category.Associative (associate, Associative (disassociate))
import Control.Category.Monoidal
import Control.Lens (Iso', Prism', lazy, lens, reversed, _Just, _Left, _Right)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum (DSum((:=>)) )
import Data.Either (isLeft, isRight)
import Data.Foldable (Foldable(fold))
import Data.Functor.Identity (Identity)
import Data.Int (Int8)
import Data.Map (Map)
import Data.Some (Some)
import Data.Tabulation (HasFields(Field, tabulateFieldsA, fieldLens))
import Data.Text (Text)
import Data.Universe (Finite(universeF), Universe)
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Control.Categorical.Functor as Cat
import qualified Data.Aeson as Aeson
import qualified Data.Dependent.Map as DMap
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty (defaultMain, testGroup, TestName, TestTree)
import Test.Tasty.QuickCheck (testProperty)

import Obelisk.Route
import Obelisk.Route.TH

data Input
  = Input_Word Word
  | Input_Text Text
  | Input_Pair Input Input
  | Input_List [Input]
  deriving (Eq, Ord, Read, Show, Generic, Aeson.FromJSON, Aeson.ToJSON)

instance Arbitrary Input where
  arbitrary = oneof
    [ Input_Word <$> arbitrary
    , Input_Text <$> arbitrary
    , Input_Pair <$> arbitrary <*> arbitrary
    , Input_List <$> (vector =<< chooseInt (0,2))
    ]
  shrink = \case
    Input_Word a   -> Input_Word <$> shrink a
    Input_Text a   -> Input_Text <$> shrink a
    Input_Pair a b -> Input_Pair <$> shrink a <*> shrink b
    Input_List a   -> Input_List <$> shrink a

data XY = XY
  { _x :: Int
  , _y :: Word
  } deriving (Eq, Ord, Show)

instance Arbitrary XY where
  arbitrary = XY <$> arbitrary <*> arbitrary

data XYField a where
  XYField_X :: XYField Int
  XYField_Y :: XYField Word

instance HasFields XY where
  type Field XY = XYField
  fieldLens = \case
    XYField_X -> lens _x $ \xy x -> xy { _x = x }
    XYField_Y -> lens _y $ \xy y -> xy { _y = y }
  tabulateFieldsA g = pure XY
    <*> g XYField_X
    <*> g XYField_Y

deriveRouteComponent ''XYField


instance Arbitrary (R XYField) where
  arbitrary = oneof
    [ fmap (XYField_X :=>) arbitrary
    , fmap (XYField_Y :=>) arbitrary
    ]
instance Arbitrary (DMap XYField Identity) where
  arbitrary = fmap fold $ sequence
    [ opt XYField_X 1
    , opt XYField_Y 1
    ] where opt k v = oneof $ fmap pure [ mempty, DMap.singleton k v ]

data A = A deriving (Bounded, Enum, Eq, Ord, Show, Universe)
data B = B deriving (Bounded, Enum, Eq, Ord, Show, Universe)
data C = C1 | C2 deriving (Bounded, Enum, Eq, Ord, Show, Universe)
instance Arbitrary A where arbitrary = pure A
instance Arbitrary B where arbitrary = pure B

ac :: Encoder' A C
ac = generalizeIdentity $ handleEncoder (\_ -> A) $ enumEncoder $ \A -> C1

bc :: Encoder' B C
bc = enumEncoder $ \B -> C2

type Encoder' a b = Encoder (Either Text) (Either Text) a b
type Cont a = forall r. (a -> r) -> r
type RoundtripConstraints a = (Arbitrary a, Eq a, Show a)
data Ex where
  Ex :: RoundtripConstraints x => Encoder' x y -> Ex


roundtripsProp :: Eq a => Encoder Identity (Either Text) a b -> a -> Bool
roundtripsProp e a = tryDecode e (encode e a) == pure a

withCheckedEncoder
  :: Testable prop
  => Encoder' a b
  -> (Encoder Identity (Either Text) a b -> prop)
  -> Property
withCheckedEncoder e f = case checkEncoder e of
  Left _ -> property False
  Right e' -> property $ f e'

mkRoundtripTestTree :: (Arbitrary a, Show a, RoundtripConstraints x) => TestName -> (a -> Encoder' x y) -> TestTree
mkRoundtripTestTree lbl f = testProperty lbl $ withMaxSuccess 1e3 $ \(a, x) -> withCheckedEncoder (f x) (flip roundtripsProp a)

arity0 :: Cont (forall a b. RoundtripConstraints a => TestName -> Encoder' a b -> TestTree)
arity0 f = f $ \lbl e -> mkRoundtripTestTree lbl $ \() -> e

arity1 :: Cont (forall a b x. (RoundtripConstraints a, Arbitrary x, Show x) => TestName -> (x -> Encoder' a b) -> TestTree)
arity1 f = f mkRoundtripTestTree

withEncoders
  :: [(TestName, Ex)]
  -> (forall x y. RoundtripConstraints x => TestName -> Encoder' x y -> t)
  -> [t]
withEncoders es t = flip fmap es $ \(lbl, Ex e) -> t lbl e

withEncoders2
  :: [(TestName, Ex)]
  -> [(TestName, Ex)]
  -> (forall a0 a1 b0 b1. (RoundtripConstraints a0, RoundtripConstraints b0) => TestName -> Encoder' a0 a1 -> Encoder' b0 b1 -> t)
  -> [t]
withEncoders2 xs ys f = liftA2 g xs ys
  where g (n1, Ex e1) (n2, Ex e2) = f (n1 <> "," <> n2) e1 e2


unsafeShowShadowEncoder :: (Universe a, Read a, Read b, Show a, Show b) => Encoder' (Either a b) PageName
unsafeShowShadowEncoder = shadowEncoder unsafeShowEncoder unsafeShowEncoder

xymapEncoder :: Encoder' (DMap XYField Identity) (Map Text Text)
xymapEncoder = dmapEncoder k v
  where
    k :: Encoder' (Some XYField) Text
    k = enum1Encoder $ \case
      XYField_X -> "x"
      XYField_Y -> "y"
    v :: XYField a -> Encoder' a Text
    v = \case
      XYField_X -> unsafeTshowEncoder
      XYField_Y -> unsafeTshowEncoder

xypathFieldEncoder :: Encoder' (XY, [Text]) [Text]
xypathFieldEncoder = pathFieldEncoder $ \case
  XYField_X -> unsafeTshowEncoder
  XYField_Y -> unsafeTshowEncoder

fragmentEncoder, overlappingFragmentEncoder :: Encoder' (R XYField) PageName
(fragmentEncoder, overlappingFragmentEncoder) = (enc "int" "word", enc "tag" "tag")
  where
    enc :: Text -> Text -> Encoder' (R XYField) PageName
    enc i w = pathComponentEncoder $ \case
      XYField_X -> PathSegment i unsafeShowEncoder
      XYField_Y -> PathSegment w unsafeShowEncoder

-- No arguments
atomicEncoders :: [(TestName, Ex)]
atomicEncoders = let t n e = (n, Ex e) in
  [ t "addPathSegmentEncoder" addPathSegmentEncoder
  , t "fieldMapEncoder" $ fieldMapEncoder @_ @_ @XY
  , t "jsonEncoder" $ jsonEncoder @_ @_ @Input
  , t "maybeToEitherEncoder" $ maybeToEitherEncoder @_ @_ @Input
  , t "pathComponentEncoder" fragmentEncoder
  , t "pathSegmentsTextEncoder" pathSegmentsTextEncoder
  , t "singletonListEncoder" $ singletonListEncoder @_ @_ @Input
  , t "toListMapEncoder" $ toListMapEncoder @_ @_ @Input @Input
  , t "unsafeTshowEncoder" $ unsafeTshowEncoder @Input

  --, t "consEncoder" $ consEncoder @_ @_ @Word                               -- failing/unexported
  --, t "listToNonEmptyEncoder" (listToNonEmptyEncoder @_ @_ @Text)           -- failing
  --, t "pathOnlyEncoderIgnoringQuery" pathOnlyEncoderIgnoringQuery           -- unexported
  --, t "pathQueryEncoder" pathQueryEncoder                                   -- failing
  --, t "queryParametersTextEncoder" queryParametersTextEncoder               -- failing

  --, t "someConstEncoder" (someConstEncoder @_ @_ @Input)                    -- Eq (Some (Const a)) requires GEq (Const a)
  --, t "someSumEncoder" (someSumEncoder @_ @_ @(Const Input) @(Const Input)) -- Eq (Some (Const a)) requires GEq (Const a)

  , t "associate"    $ associate    @_ @(,)    @Bool @Text @Word
  , t "associate"    $ associate    @_ @Either @Bool @Text @Word
  , t "disassociate" $ disassociate @_ @(,)    @Bool @Text @Word
  , t "disassociate" $ disassociate @_ @Either @Bool @Text @Word

  , t "idl"   $ idl   @_ @(,) @Text
  , t "idr"   $ idr   @_ @(,) @Text
  , t "coidl" $ coidl @_ @(,) @Text
  , t "coidr" $ coidr @_ @(,) @Text
  ]

-- No encoders as arguments
primitiveEncoders :: [(TestName, Ex)]
primitiveEncoders = fold
  [ atomicEncoders
  , reviews
  , views
  , [ t "enumEncoder" $ enumEncoder @_ @_ @Word8 (+1) ]
  ]
  where
    t n e = (n, Ex e)

    r :: (forall x y. RoundtripConstraints x => TestName -> Prism' y x -> (TestName, Ex))
    r n p = t ("reviewEncoder: " <> n) (reviewEncoder p)

    v :: (forall x y. RoundtripConstraints x => TestName -> Iso' x y -> (TestName, Ex))
    v n p = t ("viewEncoder: " <> n) (viewEncoder p)

    reviews =
      [ r @Input "_Just"  _Just
      , r @Input "_Left"  _Left
      , r @Input "_Right" _Right
      ]

    views =
      [ v @Input  "id"       id
      , v @Text   "lazy"     lazy
      , v @String "reversed" reversed
      ]

exhaustive :: TestTree
exhaustive =
  let
    prop :: Cont (forall a b. (Eq a, Finite a) => TestName -> Encoder' a b -> TestTree)
    prop f = f $ \lbl e -> testProperty lbl $ withCheckedEncoder e $ flip all universeF . roundtripsProp
  in
    testGroup "Roundtrip" $ prop $ \t ->
      [ t "voidEncoder" voidEncoder
      , t "void1Encoder" void1Encoder
      , t "id (Word8)" $ id @_ @Word8
      , t "enumEncoder" $ enumEncoder @_ @_ @Word8 (+1)
      ]

overlaps :: TestTree
overlaps =
  let
    prop :: (forall x y. Either x y -> Bool) -> Cont (forall a b. TestName -> Encoder' a b -> TestTree)
    prop is f = f $ \n -> testProperty n . is . checkEncoder @(Either Text)

  in
    testGroup "Overlaps"
      [ testGroup "No false positives" $ prop isRight $ \_t ->
        [ -- t "shadowEncoder" $ shadowEncoder bc ac -- https://github.com/obsidiansystems/obelisk/pull/987
        ]
      , testGroup "No false negatives" $ prop isLeft $ \t ->
        [ t "enumEncoder" $ enumEncoder @_ @_ @Word8 (*2)
        , t "pathComponentEncoder" overlappingFragmentEncoder
        , t "shadowEncoder" $ unsafeShowShadowEncoder @Word8 @Int8
        , t "shadowEncoder" $ unsafeShowShadowEncoder @Word8 @Word8
        ]
      ]

roundtrips :: TestTree
roundtrips = testGroup "Roundtrip" $ fold
  [ arity0 $ withEncoders primitiveEncoders
  , arity0 $ \t ->
    [ t "dmapEncoder" xymapEncoder
    , t "pathFieldEncoder" xypathFieldEncoder
    , t "shadowEncoder" $ unsafeShowShadowEncoder @Word8 @Char
    --, t "shadowEncoder" $ shadowEncoder ac bc --https://github.com/obsidiansystems/obelisk/pull/987
    , t "handleEncoder" $ generalizeIdentity $ handleEncoder @_ @_ @Input (error "Must not be used") id
    ]
  , arity1 $ \t ->
    [ t "unitEncoder" (unitEncoder @_ @_ @Input)
    --, t "joinPairTextEncoder" joinPairTextEncoder              -- Failing
    , t "prefixTextEncoder" prefixTextEncoder
    , t "prefixNonemptyTextEncoder" prefixNonemptyTextEncoder
    ]
  , arity0 $ \t ->
    [ testGroup "left identity"  $ withEncoders primitiveEncoders $ \lbl e -> t lbl $ id . e
    , testGroup "right identity" $ withEncoders primitiveEncoders $ \lbl e -> t lbl $ e . id
    , testGroup "fmap"
      [ testGroup "Maybe"  $ withEncoders primitiveEncoders $ \lbl -> t lbl . Cat.fmap @Maybe
      , testGroup "Either" $ withEncoders primitiveEncoders $ \lbl -> t lbl . Cat.fmap @(Either ())
      ]
    , let
        sampleSize = ceiling @Double @Int . sqrt . fromIntegral . length
        smallSample = take (sampleSize primitiveEncoders) primitiveEncoders
      in
        testGroup "bimap"
          [ testGroup "(,)"    $ withEncoders2 smallSample smallSample $ \lbl e1 e2 -> t lbl $ bimap @(,)    e1 e2
          , testGroup "Either" $ withEncoders2 smallSample smallSample $ \lbl e1 e2 -> t lbl $ bimap @Either e1 e2
          ]
    ]
  ]

tests :: IO ()
tests = do
  defaultMain $ testGroup "Encoders"
    [ testGroup "Exhaustive search" [ exhaustive ]
    , testGroup "Unit testing" [ overlaps ]
    , testGroup "Property testing" [ roundtrips ]
    ]

main :: IO ()
main = tests

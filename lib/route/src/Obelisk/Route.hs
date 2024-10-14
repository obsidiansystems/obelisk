{-# LANGUAGE CPP #-}
{-|

Types and functions for defining routes and 'Encoder's.

-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Obelisk.Route
  ( -- * Primary Types
    R
  , PageName
  , PathQuery
  , Encoder
  , EncoderImpl (..)
  , EncoderFunc (..)

  -- * Patterns, operators, and utilities
  , (:.)
  , (?/)
  , hoistR
  , pattern (:.)
  , pattern (:/)
  , unsafeEncoder
  , checkEncoder
  , unsafeMkEncoder
  , encode
  , decode
  , tryDecode
  , hoistCheck
  , hoistParse
  , generalizeIdentity
  , mapSome
  , rPrism
  , _R
  , renderObeliskRoute
  , renderBackendRoute
  , renderFrontendRoute
  , byteStringsToPageName

  -- * Collating Routes
  , SegmentResult (..)
  , pathComponentEncoder

  , FullRoute (..)
  , _FullRoute_Frontend
  , _FullRoute_Backend
  , mkFullRouteEncoder

  , ObeliskRoute (..)
  , _ObeliskRoute_App
  , _ObeliskRoute_Resource
  , ResourceRoute (..)

  , JSaddleWarpRoute (..)
  , jsaddleWarpRouteEncoder

  , IndexOnlyRoute (..)
  , indexOnlyRouteSegment
  , indexOnlyRouteEncoder

  -- * Provided Encoders
  , enumEncoder
  , enum1Encoder
  , checkEnum1EncoderFunc
  , unitEncoder
  , pathOnlyEncoder
  , addPathSegmentEncoder
  , pathParamEncoder
  , pathLiteralEncoder
  , singletonListEncoder
  , packTextEncoder
  , unpackTextEncoder
  , prefixTextEncoder
  , unsafeTshowEncoder
  , unsafeShowEncoder
  , readShowEncoder
  , someConstEncoder
  , singlePathSegmentEncoder
  , maybeEncoder
  , maybeToEitherEncoder
  , justEncoder
  , nothingEncoder
  , isoEncoder
  , viewEncoder
  , wrappedEncoder
  , unwrappedEncoder
  , listToNonEmptyEncoder
  , prefixNonemptyTextEncoder
  , joinPairTextEncoder
  , toListMapEncoder
  , shadowEncoder
  , prismEncoder
  , reviewEncoder
  , obeliskRouteEncoder
  , obeliskRouteSegment
  , pageNameEncoder
  , pathQueryEncoder
  , handleEncoder
  , someSumEncoder
  , Void1
  , void1Encoder
  , pathSegmentsTextEncoder
  , queryParametersTextEncoder
  , integralEncoder
  , pathSegmentEncoder
  , queryOnlyEncoder
  , Decoder(..)
  , dmapEncoder
  , fieldMapEncoder
  , pathFieldEncoder
  , jsonEncoder
  , utf8Encoder
  ) where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category (Category (..))
import qualified Control.Categorical.Functor as Cat
import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Monoidal
import Control.Category.Braided
import Control.Lens
  ( Identity (..)
  , (^.)
  , (^?)
  , _Just
  , _Nothing
  , Cons(..)
  , from
  , imap
  , iso
  , Iso'
  , itraverse
  , makePrisms
  , Prism'
  , prism'
  , re
  , review
  , view
  , Wrapped (..)
  )

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Control.Monad.Trans (lift)
import Data.Monoid ((<>))
#endif
#endif

import Control.Monad.Except
import qualified Control.Monad.State.Strict as State
import Control.Monad.Writer (execWriter, tell)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import Data.Either.Validation (Validation (..))
import Data.Foldable
import Data.Functor (($>))
import Data.Functor.Sum
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid (Ap(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Semigroupoid
import Data.Some (Some(Some), foldSome, mapSome)
import Data.Tabulation
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Lens (IsText, packed, unpacked)
import Data.Type.Equality
import Data.Universe
import Data.Universe.Some
import Network.HTTP.Types.URI
import qualified Numeric.Lens
import Obelisk.Route.TH
import Text.Read (readMaybe)

-- Design goals:
-- No start-up time on the frontend (not yet met)
-- Able to ensure that there aren't overlapping routes prior to deployment
-- Easy to write common types of parsers
--   Completeness checking

-- Laws:
-- We statically know that all valid Routes can be turned into valid URIs
--   - This means we need to know whether there are any overlaps, e.g. with the static file namespace

--TODO:
-- Backend:
--  Redirect the user to a canonical route
--  Pre-render
-- Frontend:
--  Intercept links that can be kept within the app
--  Fragments

--------------------------------------------------------------------------------
-- Subroutes/paths
--------------------------------------------------------------------------------

-- | This alias is used to wrap the type of a route GADT so that the type variable of the GADT is existentially quantified.
--
-- Given the following route type :
--
-- @
--
-- data MyRoutes a where
--   MyRoutes_Main :: MyRoutes ()
--   MyRoutes_A :: MyRoutes Text
--   MyRoutes_B :: MyRoutes Int
-- @
--
-- Using 'R' we're able to write type signatures without worrying about the 'a':
--
-- @
-- myRoutesWidget :: RoutedT t (R MyRoutes) m ()
-- @
--
type R f = DSum f Identity --TODO: Better name

-- | Convenience builder for an 'R' using 'Identity' for the functor.
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b
{-# COMPLETE (:/) #-}
infixr 5 :/

-- | Like '(:/)' but adds a 'Just' wrapper around the right-hand side.
(?/) :: f (Maybe a) -> a -> R f
r ?/ a = r :/ Just a
infixr 5 ?/

hoistR :: (forall x. f x -> g x) -> R f -> R g
hoistR f (x :=> Identity y) = f x :/ y

--------------------------------------------------------------------------------
-- Dealing with pairs (i.e. non-dependently-typed subroutes/paths)
--------------------------------------------------------------------------------

infixr 5 :.
type (:.) = (,)

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 810
{-# COMPLETE (:.) #-}
#else
{-# WARNING (:.)
  [ "Use of this pattern in GHC < 8.10 will result in spurious non-exhaustive warnings at every use site."
  , "We cannot provide a COMPLETE pragma to silence these due to a GHC bug: https://gitlab.haskell.org/ghc/ghc/issues/17729."
  , "The bug hides incompleteness warnings for all two tuples when the COMPLETE pattern is in scope."
  , "Instead, you should use (,) directly until you can switch to GHC >= 8.10, where the COMPLETE pragma is reinstated."
  ]
  #-}
#endif
#endif
pattern (:.) :: a -> b -> a :. b
pattern a :. b = (a, b)

addPathSegmentEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse (Text, PageName) PageName
addPathSegmentEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(ph, (pt, q)) -> (ph : pt, q)
  , _encoderImpl_decode = \(p, q) -> case p of
      [] -> throwError "Expected a path segment"
      ph : pt -> pure (ph, (pt, q))
  }

pathParamEncoder
  :: forall check parse item rest.
     ( Applicative check
     , MonadError Text parse
     )
  => Encoder check parse item Text
  -> Encoder check parse rest PageName
  -> Encoder check parse (item :. rest) PageName
pathParamEncoder itemUnchecked restUnchecked = addPathSegmentEncoder . bimap itemUnchecked restUnchecked

pathLiteralEncoder
  :: ( Applicative check
     , MonadError Text parse
     )
  => Text
  -> Encoder check parse a PageName
  -> Encoder check parse a PageName
pathLiteralEncoder t e = addPathSegmentEncoder . bimap (unitEncoder t) e . coidl

--------------------------------------------------------------------------------
-- Encoder fundamentals
--------------------------------------------------------------------------------

-- | This is the type of route encoder/decoders. It is parameterised over two monads: Firstly, the monad
-- used to check the validity of the encoder (i.e. that it is total), secondly the monad used for parsing
-- during the decode phase. The following two parameters are respectively the type of decoded data, and the
-- encoded type.
newtype Encoder check parse decoded encoded =
  Encoder { unEncoder :: check (EncoderImpl parse decoded encoded) }

unsafeEncoder :: check (EncoderImpl parse decoded encoded) -> Encoder check parse decoded encoded
unsafeEncoder = Encoder

-- | The internal type used to construct primitive 'Encoder' values.
-- Law:
-- forall p. _encoderImpl_decode ve . _encoderImpl_encode ve p == pure
-- Note that the reverse may not be the case: when parsing, a route may be canonicalized, and erroneous routes may be collapsed to a single 404 route.  However, as a consequence of the law, encode . decode must be idempotent.
data EncoderImpl parse decoded encoded = EncoderImpl
  { _encoderImpl_decode :: !(encoded -> parse decoded) -- Can fail; can lose information; must always succeed on outputs of `_encoderImpl_encode` and result in the original value
  , _encoderImpl_encode :: !(decoded -> encoded) -- Must be injective
  }

-- | Once an 'Encoder' has been checked, so that its check monad has become 'Identity', and its parser is total
-- so that its parse monad is also 'Identity', it may be used to actually decode by applying this function.
decode :: Encoder Identity Identity decoded encoded -> encoded -> decoded
decode e x = runIdentity (tryDecode e x)

-- | Once an 'Encoder' has been checked, so that its check monad has become 'Identity', even if the same is not true of the
-- parse monad, we may still attempt to decode with it in its parse monad.
tryDecode :: Encoder Identity parse decoded encoded -> encoded -> parse decoded
tryDecode (Encoder (Identity impl)) = _encoderImpl_decode impl

-- | Similar to 'decode', once an encoder has been checked so that its check monad is Identity, it
-- can be used to actually encode by using this. Note that while there's no constraint on the parse monad here,
-- one should usually be applying decode and encode to the same 'Encoder'
encode :: Encoder Identity parse decoded encoded -> decoded -> encoded
encode (Encoder (Identity impl)) = _encoderImpl_encode impl

-- | This is a primitive used to build encoders which can't fail to check. It should not be used unless one is
-- reasonably certain that the law given for 'EncoderImpl' above holds.
unsafeMkEncoder :: (Applicative check) => EncoderImpl parse decoded encoded -> Encoder check parse decoded encoded
unsafeMkEncoder impl = Encoder (pure impl)

-- | Transform the check monad of an 'Encoder' by applying a natural transformation.
hoistCheck :: (forall t. check t -> check' t) -> Encoder check parse a b -> Encoder check' parse a b
hoistCheck f (Encoder x) = Encoder (f x)

-- | Transform the parse monad of an 'Encoder' by applying a natural transformation.
hoistParse :: (Functor check)
  => (forall t. parse t -> parse' t) -> Encoder check parse a b -> Encoder check parse' a b
hoistParse f (Encoder x) = Encoder (fmap (\(EncoderImpl dec enc) -> EncoderImpl (f . dec) enc) x)

generalizeIdentity
  :: (Functor check, Applicative parse)
  => Encoder check Identity a b
  -> Encoder check parse a b
generalizeIdentity = hoistParse (pure . runIdentity)

-- | Check an 'Encoder', transforming it into one whose check monad is anything we want (usually Identity).
checkEncoder :: (Applicative check', Functor check)
  => Encoder check parse decoded encoded
  -> check (Encoder check' parse decoded encoded)
checkEncoder = fmap unsafeMkEncoder . unEncoder

instance (Applicative check, Monad parse) => Semigroupoid (Encoder check parse) where
  Encoder f `o` Encoder g = Encoder $ liftA2 (.) f g

instance (Applicative check, Monad parse) => Category (Encoder check parse) where
  id = Encoder $ pure id
  (.) = o

instance Monad parse => Category (EncoderImpl parse) where
  id = EncoderImpl
    { _encoderImpl_decode = pure
    , _encoderImpl_encode = id
    }
  f . g = EncoderImpl
    { _encoderImpl_decode = _encoderImpl_decode g <=< _encoderImpl_decode f
    , _encoderImpl_encode = _encoderImpl_encode f . _encoderImpl_encode g
    }

instance Monad parse => PFunctor (,) (EncoderImpl parse) (EncoderImpl parse) where
  first f = bimap f id
instance Monad parse => QFunctor (,) (EncoderImpl parse) (EncoderImpl parse) where
  second g = bimap id g
instance Monad parse => Bifunctor (,) (EncoderImpl parse) (EncoderImpl parse) (EncoderImpl parse) where
  bimap f g = EncoderImpl
    { _encoderImpl_encode = bimap (_encoderImpl_encode f) (_encoderImpl_encode g)
    , _encoderImpl_decode = \(a, b) -> liftA2 (,) (_encoderImpl_decode f a) (_encoderImpl_decode g b)
    }

instance (Monad parse, Applicative check) => Braided (Encoder check parse) (,) where
  braid = viewEncoder (iso swap swap)


instance (Applicative check, Monad parse) => PFunctor (,) (Encoder check parse) (Encoder check parse) where
  first f = bimap f id
instance (Applicative check, Monad parse) => QFunctor (,) (Encoder check parse) (Encoder check parse) where
  second g = bimap id g
instance (Applicative check, Monad parse) => Bifunctor (,) (Encoder check parse) (Encoder check parse) (Encoder check parse) where
  bimap f g = Encoder $ liftA2 bimap (unEncoder f) (unEncoder g)

instance (Traversable f, Monad parse) => Cat.Functor f (EncoderImpl parse) (EncoderImpl parse) where
  fmap ve = EncoderImpl
    { _encoderImpl_encode = fmap $ _encoderImpl_encode ve
    , _encoderImpl_decode = traverse $ _encoderImpl_decode ve
    }

instance Monad parse => PFunctor Either (EncoderImpl parse) (EncoderImpl parse) where
  first f = bimap f id
instance Monad parse => QFunctor Either (EncoderImpl parse) (EncoderImpl parse) where
  second g = bimap id g
instance Monad parse => Bifunctor Either (EncoderImpl parse) (EncoderImpl parse) (EncoderImpl parse) where
  bimap f g = EncoderImpl
    { _encoderImpl_encode = bimap (_encoderImpl_encode f) (_encoderImpl_encode g)
    , _encoderImpl_decode = \case
      Left a -> Left <$> _encoderImpl_decode f a
      Right b -> Right <$> _encoderImpl_decode g b
    }

instance (Monad parse, Applicative check) => QFunctor Either (Encoder check parse) (Encoder check parse) where
  second g = bimap id g
instance (Monad parse, Applicative check) => PFunctor Either (Encoder check parse) (Encoder check parse) where
  first f = bimap f id
instance (Monad parse, Applicative check) => Bifunctor Either (Encoder check parse) (Encoder check parse) (Encoder check parse) where
  bimap f g = Encoder $ liftA2 bimap (unEncoder f) (unEncoder g)

instance (Applicative check, Monad parse) => Associative (Encoder check parse) Either where
  associate = viewEncoder (iso (associate @(->) @Either) disassociate)
  disassociate = viewEncoder (iso disassociate associate)

instance (Monad parse, Applicative check) => Braided (Encoder check parse) Either where
  braid = viewEncoder (iso swap swap)



instance (Traversable f, Monad check, Monad parse) => Cat.Functor f (Encoder check parse) (Encoder check parse) where
  fmap e = Encoder $ do
    ve <- unEncoder e
    pure $ Cat.fmap ve

instance Monad parse => Associative (EncoderImpl parse) (,) where
  associate = EncoderImpl
    { _encoderImpl_encode = associate
    , _encoderImpl_decode = pure . disassociate
    }
  disassociate = EncoderImpl
    { _encoderImpl_encode = disassociate
    , _encoderImpl_decode = pure . associate
    }

instance Monad parse => Monoidal (EncoderImpl parse) (,) where
  type Id (EncoderImpl parse) (,) = ()
  idl = EncoderImpl
    { _encoderImpl_encode = idl
    , _encoderImpl_decode = pure . coidl
    }
  idr = EncoderImpl
    { _encoderImpl_encode = idr
    , _encoderImpl_decode = pure . coidr
    }
  coidl = EncoderImpl
    { _encoderImpl_encode = coidl
    , _encoderImpl_decode = pure . idl
    }
  coidr = EncoderImpl
    { _encoderImpl_encode = coidr
    , _encoderImpl_decode = pure . idr
    }

instance (Applicative check, Monad parse) => Associative (Encoder check parse) (,) where
  associate = Encoder $ pure associate
  disassociate = Encoder $ pure disassociate

instance (Applicative check, Monad parse) => Monoidal (Encoder check parse) (,) where
  type Id (Encoder check parse) (,) = ()
  idl = Encoder $ pure idl
  idr = Encoder $ pure idr
  coidl = Encoder $ pure coidl
  coidr = Encoder $ pure coidr

--------------------------------------------------------------------------------
-- Specific instances of encoders
--------------------------------------------------------------------------------

-- | Given a valid 'Iso' from lens, construct an 'Encoder'
viewEncoder :: (Applicative check, Applicative parse) => Iso' a b -> Encoder check parse a b
viewEncoder f = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = view f
  , _encoderImpl_decode = pure . view (from f)
  }

wrappedEncoder :: (Wrapped a, Applicative check, Applicative parse) => Encoder check parse (Unwrapped a) a
wrappedEncoder = viewEncoder $ from _Wrapped'

unwrappedEncoder :: (Wrapped a, Applicative check, Applicative parse) => Encoder check parse a (Unwrapped a)
unwrappedEncoder = viewEncoder $ _Wrapped'

maybeToEitherEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Maybe a) (Either () a)
maybeToEitherEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \case
      Nothing -> Left ()
      Just a -> Right a
  , _encoderImpl_decode = pure . \case
      Left _ -> Nothing
      Right a -> Just a
  }

maybeEncoder
  :: ( MonadError Text check
     , Show a
     , Show b
     , check ~ parse
     )
  => Encoder check parse () b
  -> Encoder check parse a b
  -> Encoder check parse (Maybe a) b
maybeEncoder f g = shadowEncoder f g . maybeToEitherEncoder

-- | Encode a value by simply applying 'Just'
justEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse a (Maybe a)
justEncoder = reviewEncoder _Just

-- | Encode () to 'Nothing'.
nothingEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse () (Maybe a)
nothingEncoder = reviewEncoder _Nothing

someConstEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Some (Const a)) a
someConstEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(Some (Const a)) -> a
  , _encoderImpl_decode = pure . Some . Const
  }

-- | WARNING: This is only safe if the Show and Read instances for 'a' are inverses of each other
--
-- Instances must be able to satisfy the following property for this 'Encoder' to be safe:
--
-- @
-- forall a. reads (show a) === [(a, "")]
-- @
--
unsafeTshowEncoder :: (Show a, Read a, Applicative check, MonadError Text parse) => Encoder check parse a Text
unsafeTshowEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = tshow
  , _encoderImpl_decode = \raw -> case readMaybe $ T.unpack raw of
      Nothing -> throwError $ "unsafeTshowEncoderImpl: couldn't decode " <> tshow raw
      Just parsed -> pure parsed
  }

newtype EncoderFunc check parse p r = EncoderFunc { runEncoderImplFunc :: forall a. p a -> Encoder check parse a r }

newtype Flip f a b = Flip { unFlip :: f b a }

checkEnum1EncoderFunc
  :: forall check check' parse p r.
     ( Universe (Some p)
     , GCompare p
     , Monad check
     , Applicative check'
     )
  => (forall a. p a -> Encoder check parse a r)
  -> check (EncoderFunc check' parse p r)
checkEnum1EncoderFunc f = do
  (encoderImpls :: DMap p (Flip (EncoderImpl parse) r)) <- DMap.fromList <$>
    traverse (\(Some p) -> (p :=>) . Flip <$> unEncoder (f p)) universe
  pure $ EncoderFunc $ \p -> unsafeMkEncoder . unFlip $
    DMap.findWithDefault (error "checkEnum1EncoderFunc: EncoderImpl not found (should be impossible)") p encoderImpls

-- | This type is used by pathComponentEncoder to allow the user to indicate how to treat
-- various cases when encoding a dependent sum of type `(R p)`.
data SegmentResult check parse a =
    PathEnd (Encoder check parse a (Map Text (Maybe Text)))
    -- ^ Indicate that the path is finished, with an Encoder that translates the
    -- corresponding value into query parameters
  | PathSegment Text (Encoder check parse a PageName)
    -- ^ Indicate that the key should be represented by an additional path segment with
    -- the given 'Text', and give an Encoder for translating the corresponding value into
    -- the remainder of the route.

-- | Encode a dependent sum of type `(R p)` into a PageName (i.e. the path and query part of a URL) by using the
-- supplied function to decide how to encode the constructors of p using the SegmentResult type. It is important
-- that the number of values of type `(Some p)` be relatively small in order for checking to complete quickly.
pathComponentEncoder
  :: forall check parse p.
     ( Universe (Some p)
     , GShow p
     , GCompare p
     , MonadError Text check
     , MonadError Text parse )
  => (forall a. p a -> SegmentResult check parse a)
  -> Encoder check parse (R p) PageName
pathComponentEncoder f = Encoder $ do
  let extractEncoder = \case
        PathEnd e -> first (unitEncoder []) . coidl . e
        PathSegment _ e -> e
      extractPathSegment = \case
        PathEnd _ -> Nothing
        PathSegment t _ -> Just t
  EncoderFunc f' <- checkEnum1EncoderFunc (extractEncoder . f)
  unEncoder (pathComponentEncoderImpl (enum1Encoder (extractPathSegment . f)) f')

pathComponentEncoderImpl :: forall check parse p. (Monad check, Monad parse)
  => Encoder check parse (Some p) (Maybe Text)
  -> (forall a. p a -> Encoder Identity parse a PageName)
  -> Encoder check parse (R p) PageName
pathComponentEncoderImpl =
  chainEncoder (lensEncoder (\(_, b) a -> (a, b)) Prelude.fst consEncoder)

--NOTE: Naming convention in this module is to always talk about things in the *encoding* direction, never in the *decoding* direction

chainEncoder
  :: forall check parse p r b.
     ( Monad check
     , Monad parse
     )
  => Encoder check parse (b, r) r
  -> Encoder check parse (Some p) b
  -> (forall a. p a -> Encoder Identity parse a r)
  -> Encoder check parse (R p) r
chainEncoder cons this rest = Encoder $ do
  consValid <- unEncoder cons
  thisValid <- unEncoder this
  pure $ EncoderImpl
    { _encoderImpl_decode = \v -> do
        (here, following) <- _encoderImpl_decode consValid v
        _encoderImpl_decode thisValid here >>= \case
          Some r ->
            (r :/) <$> _encoderImpl_decode (runIdentity . unEncoder $ rest r) following
    , _encoderImpl_encode = \(r :/ s) ->
        _encoderImpl_encode consValid
          ( _encoderImpl_encode thisValid $ Some r
          , _encoderImpl_encode (runIdentity . unEncoder $ rest r) s)
    }

--TODO: Do this in terms of a lens instead
lensEncoder :: (Applicative check, Monad parse)
  => (b -> [a] -> b) -> (b -> [a]) -> Encoder check parse (c, [a]) [a] -> Encoder check parse (c, b) b
lensEncoder set get g = Encoder $ do
  gImpl <- unEncoder g
  pure $ EncoderImpl
    { _encoderImpl_encode = \(ma, b) -> set b $ _encoderImpl_encode gImpl (ma, get b)
    , _encoderImpl_decode = \b -> do
        (ma, la) <- _encoderImpl_decode gImpl $ get b
        pure (ma, set b la)
    }

consEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Maybe a, [a]) [a] --TODO: Really shouldn't *always* have the [a], even in the Nothing case
consEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(h, t) -> maybeToList h <> t
  , _encoderImpl_decode = pure . \case
      [] -> (Nothing, [])
      h:t -> (Just h, t)
  }

tshow :: Show a => a -> Text
tshow = T.pack . show

shadowEncoder
  :: ( Universe a
     , MonadError Text check
     , Show a
     , Show b
     , Show c
     , check ~ parse --TODO: Get rid of this
     )
  => Encoder check parse a c -- ^ Overlaps; should have a small number of possible routes
  -> Encoder check parse b c -- ^ Gets overlapped
  -> Encoder check parse (Either a b) c
shadowEncoder f g = Encoder $ do
  vf <- unEncoder f
  vg <- unEncoder g
  let gCanParse c = catchError (Just <$> _encoderImpl_decode vg c) (\_ -> pure Nothing)
  overlaps <- fmap catMaybes $ forM universe $ \a -> do
    let c = _encoderImpl_encode vf a
    mb <- gCanParse c
    pure $ fmap (\b -> (a, b, c)) mb
  case overlaps of
    [] -> pure ()
    _ -> throwError $ "shadowEncoder: overlap detected: " <> T.unlines
      (flip fmap overlaps $ \(a, b, c) -> "first encoder encodes " <> tshow a <> " as " <> tshow c <> ", which second encoder decodes as " <> tshow b)
  pure $ EncoderImpl
    { _encoderImpl_encode = \case
        Left a -> _encoderImpl_encode vf a
        Right b -> _encoderImpl_encode vg b
    , _encoderImpl_decode = \c -> (Left <$> _encoderImpl_decode vf c) `catchError` \_ -> Right <$> _encoderImpl_decode vg c
    }

enum1Encoder
  :: ( Universe (Some p)
     , GShow p
     , GCompare p
     , MonadError Text check
     , MonadError Text parse
     , Ord r
     , Show r
     )
  => (forall a. p a -> r) -> Encoder check parse (Some p) r
enum1Encoder f = enumEncoder $ \(Some p) -> f p

-- | Encode an enumerable, bounded type.  WARNING: Don't use this on types that
-- have a large number of values - it will use a lot of memory.
enumEncoder :: forall parse check p r. (Universe p, Show p, Ord p, Ord r, MonadError Text parse, MonadError Text check, Show r) => (p -> r) -> Encoder check parse p r
enumEncoder f = Encoder $ do
  let reversed = Map.fromListWith (<>) [ (f p, Set.singleton p) | p <- universe ]
      checkSingleton k vs = case Set.toList vs of
        [] -> error "enumEncoder: empty reverse mapping; should be impossible"
        [e] -> Success e
        _ -> Failure $ Map.singleton k vs
      showRedundant :: r -> Set p -> [Text]
      showRedundant k vs = ("  " <> tshow k <> " can decode to any of:")
        : fmap (("    "<>) . tshow) (Set.toList vs)
  case itraverse checkSingleton reversed :: Validation (Map r (Set p)) (Map r p) of
    Failure ambiguousEntries -> throwError $ T.unlines $
      "enumEncoder: ambiguous encodings detected:" : concat (Map.elems $ imap showRedundant ambiguousEntries)
    Success m -> pure $ EncoderImpl
      { _encoderImpl_decode = \r -> case Map.lookup r m of
          Just a -> pure a
          Nothing -> throwError $ "enumEncoder: not recognized: " <> tshow r --TODO: Report this as a better type
      , _encoderImpl_encode = f
      }

unitEncoder :: (Applicative check, MonadError Text parse, Show r, Eq r) => r -> Encoder check parse () r
unitEncoder expected = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \obtained ->
      if obtained == expected
      then pure ()
      else throwError $ "endEncoderImpl: expected " <> tshow expected <> ", got " <> tshow obtained
  , _encoderImpl_encode = \_ -> expected
  }

singlePathSegmentEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse Text PageName
singlePathSegmentEncoder = pathOnlyEncoder . singletonListEncoder

pathOnlyEncoderIgnoringQuery :: (Applicative check, MonadError Text parse) => Encoder check parse [Text] PageName
pathOnlyEncoderIgnoringQuery = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \(path, _query) -> pure path
  , _encoderImpl_encode = \path -> (path, mempty)
  }

pathOnlyEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse [Text] PageName
pathOnlyEncoder = second (unitEncoder mempty) . coidr

queryOnlyEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse (Map Text (Maybe Text)) PageName
queryOnlyEncoder = first (unitEncoder []) . coidl

singletonListEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse a [a]
singletonListEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_decode = \case
      [a] -> pure a
      l -> throwError $ "singletonListEncoderImpl: expected one item, got " <> tshow (length l)
  , _encoderImpl_encode = (:[])
  }

splitTextNonEmpty :: Text -> Text -> NonEmpty Text
splitTextNonEmpty separator v = case T.splitOn separator v of
  [] -> error "splitTextNonEmpty: Data.Text.splitOn should never return an empty list"
  h : t -> h :| t

--TODO: To know this is reversible, we must know that the separator isn't included anywhere in the input text
pathSegmentsTextEncoder :: (Applicative check, Applicative parse) => Encoder check parse (NonEmpty Text) Text
pathSegmentsTextEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = T.intercalate "/" . fmap (urlEncodeText False) . toList
  , _encoderImpl_decode = pure . fmap (urlDecodeText False) . splitTextNonEmpty "/"
  }

queryParametersTextEncoder :: (Applicative check, Applicative parse) => Encoder check parse [(Text, Maybe Text)] Text
queryParametersTextEncoder = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = \case
      [] -> ""
      params -> T.intercalate "&" (fmap encodeParameter params)
  , _encoderImpl_decode = pure . \case
      "" -> []
      encoded ->
        let h :| t = splitTextNonEmpty "&" encoded
        in fmap decodeParameter $ h : t
  }
  where
    encodeParameter (k, mv) = urlEncodeText True k <> case mv of
      Nothing -> ""
      Just v -> "=" <> urlEncodeText True v
    decodeParameter t =
      let (k, eqV) = T.breakOn "=" t
          mv = T.stripPrefix "=" eqV
      in (urlDecodeText True k, urlDecodeText True <$> mv)

urlEncodeText :: Bool -> Text -> Text
urlEncodeText q = T.decodeUtf8 . urlEncode q . T.encodeUtf8

urlDecodeText :: Bool -> Text -> Text
urlDecodeText q = T.decodeUtf8 . urlDecode q . T.encodeUtf8

listToNonEmptyEncoder :: (Applicative check, Applicative parse, Monoid a, Eq a) => Encoder check parse [a] (NonEmpty a)
listToNonEmptyEncoder = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = \case
      [] -> mempty :| []
      h : t -> h :| t
  , _encoderImpl_decode = \(h :| t) -> pure $
      if h == mempty
      then []
      else h : t
  }

prefixTextEncoder :: (Applicative check, MonadError Text parse) => Text -> Encoder check parse Text Text
prefixTextEncoder p = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = mappend p
  , _encoderImpl_decode = \v -> case T.stripPrefix p v of
      Nothing -> throwError $ "prefixTextEncoder: wrong prefix; expected " <> tshow p <> ", got " <> tshow (T.take (T.length p) v)
      Just stripped -> pure stripped
  }

prefixNonemptyTextEncoder :: (Applicative check, MonadError Text parse) => Text -> Encoder check parse Text Text
prefixNonemptyTextEncoder p = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = \case
      "" -> ""
      v -> p <> v
  , _encoderImpl_decode = \case
      "" -> pure ""
      v -> case T.stripPrefix p v of
        Nothing -> throwError $ "prefixTextEncoder: wrong prefix; expected " <> tshow p
        Just stripped -> pure stripped
  }

packTextEncoder :: (Applicative check, Applicative parse, IsText text) => Encoder check parse String text
packTextEncoder = isoEncoder packed

unpackTextEncoder :: (Applicative check, Applicative parse, IsText text) => Encoder check parse text String
unpackTextEncoder = isoEncoder unpacked

toListMapEncoder :: (Applicative check, Applicative parse, Ord k) => Encoder check parse (Map k v) [(k, v)]
toListMapEncoder = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = Map.toList
  , _encoderImpl_decode = pure . Map.fromList --TODO: Should we be stricter about repeated keys?
  }

joinPairTextEncoder :: (MonadError Text check, MonadError Text parse) => Text -> Encoder check parse (Text, Text) Text
joinPairTextEncoder = Encoder . \case
  "" -> throwError "joinPairTextEncoder: empty separator"
  separator -> pure $ EncoderImpl
    { _encoderImpl_encode = \(k, v) -> k <> separator <> v
    , _encoderImpl_decode = \r ->
        let (kt, vt) = T.breakOn separator r
        in case vt of
          -- The separator was not found
          "" -> throwError $ "joinPairTextEncoder: separator not found; expected " <> tshow separator
          _ -> return (kt, T.drop (T.length separator) vt)
    }

-- This slight generalization of 'rPrism' happens to be enough to write all our
-- prism combinators so far.
dSumPrism
  :: forall f f' g
  .  (forall a. Prism' (f a) (f' a))
  -> Prism' (DSum f g) (DSum f' g)
dSumPrism p = prism'
  (\(f' :=> x) -> f' ^. re p :=> x)
  (\(f :=> x) -> (:=> x) <$> (f ^? p))

-- already in obelisk
rPrism
  :: forall f f'
  .  (forall a. Prism' (f a) (f' a))
  -> Prism' (R f) (R f')
rPrism = dSumPrism

dSumPrism'
  :: forall f g a
  .  (forall b. Prism' (f b) (a :~: b))
  -> Prism' (DSum f g) (g a)
dSumPrism' p = dSumPrism p . iso (\(Refl :=> b) -> b) (Refl :=>)

dSumGEqPrism
  :: GEq f
  => f a
  -> Prism' (DSum f g) (g a)
dSumGEqPrism variant = dSumPrism' $ prism' (\Refl -> variant) (geq variant)

-- | Given a 'tag :: f a', make a prism for 'R f'. This generalizes the usual
-- prisms for a sum type (the ones that 'mkPrisms' would make), just as 'R'
-- generalized a usual sum type.
--
-- [This is given the '_R' name of the "cannonical" prism not because it is the
-- most general, but because it seems the most useful for routes, and 'R' itself
-- trades generality for route-specificity.]
_R
  :: GEq f
  => f a
  -> Prism' (R f) a
_R variant = dSumGEqPrism variant . iso runIdentity Identity

-- | An encoder that only works on the items available via the prism. An error will be thrown in the parse monad
-- if the prism doesn't match.
--
-- Note that a 'Prism' from @a@ to @b@ will produce an 'Encoder' from @b@ to @a@
-- (i.e. 'reviewEncoder' is a contravariant functor from the category of prisms to the category of encoders),
-- just like 'review' produces a function @b -> a@. This is because 'Prism's extract values, in a way that might
-- fail, in their forward direction and inject values, in a way that cannot fail, in their reverse direction;
-- whereas 'Encoder's encode, which cannot fail, in their forward direction, and decode, which can fail, in their
-- reverse direction. In short @reviewEncoder (f . g) = reviewEncoder g . reviewEncoder f@.
reviewEncoder :: (Applicative check, MonadError Text parse) => Prism' b a -> Encoder check parse a b
reviewEncoder p = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = review p
  , _encoderImpl_decode = \r -> case r ^? p of
      Just a -> pure a
      Nothing -> throwError "reviewEncoder: value is not present in the prism"
  }

-- | A URL path and query string, in which trailing slashes don't matter in the path
-- and duplicate query parameters are not allowed. A final goal of encoders using this library
-- will frequently be to produce this.
type PageName = ([Text], Map Text (Maybe Text))

-- | A path (separated by slashes), and a query string.
type PathQuery = (String, String)

-- | Encode a PageName into a path and query string.
pageNameEncoder :: (Applicative check, MonadError Text parse) => Encoder check parse PageName PathQuery
pageNameEncoder = bimap
  (unpackTextEncoder . prefixTextEncoder "/" . pathSegmentsTextEncoder . listToNonEmptyEncoder)
  (unpackTextEncoder . prefixNonemptyTextEncoder "?" . queryParametersTextEncoder . toListMapEncoder)

-- | Handle an error in parsing, for example, in order to redirect to a 404 page.
handleEncoder
  :: (Functor check)
  => (e -> a)
  -> Encoder check (Either e) a b
  -> Encoder check Identity a b
handleEncoder recover e = Encoder $ do
  i <- unEncoder e
  return $ i
    { _encoderImpl_decode = \a -> pure $ case _encoderImpl_decode i a of
      Right r -> r
      Left err -> recover err
    }

--------------------------------------------------------------------------------
-- Actual obelisk route info
--------------------------------------------------------------------------------

-- | The typical full route type comprising all of an Obelisk application's routes.
-- Parameterised by the top level GADTs that define backend and frontend routes, respectively.
data FullRoute :: (* -> *) -> (* -> *) -> * -> * where
  FullRoute_Backend :: br a -> FullRoute br fr a
  FullRoute_Frontend :: ObeliskRoute fr a -> FullRoute br fr a

instance (GShow br, GShow fr) => GShow (FullRoute br fr) where
  gshowsPrec p = \case
    FullRoute_Backend x -> showParen (p > 10) (showString "FullRoute_Backend " . gshowsPrec 11 x)
    FullRoute_Frontend x -> showParen (p > 10) (showString "FullRoute_Frontend " . gshowsPrec 11 x)

instance (GEq br, GEq fr) => GEq (FullRoute br fr) where
  geq (FullRoute_Backend x) (FullRoute_Backend y) = geq x y
  geq (FullRoute_Frontend x) (FullRoute_Frontend y) = geq x y
  geq _ _ = Nothing

instance (GCompare br, GCompare fr) => GCompare (FullRoute br fr) where
  gcompare (FullRoute_Backend _) (FullRoute_Frontend _) = GLT
  gcompare (FullRoute_Frontend _) (FullRoute_Backend _) = GGT
  gcompare (FullRoute_Backend x) (FullRoute_Backend y) = gcompare x y
  gcompare (FullRoute_Frontend x) (FullRoute_Frontend y) = gcompare x y

instance  (UniverseSome br, UniverseSome fr) => UniverseSome (FullRoute br fr) where
  universeSome = [Some (FullRoute_Backend x) | Some x <- universeSome]
              ++ [Some (FullRoute_Frontend x) | Some x <- universeSome]

-- | Build the typical top level application route encoder from a route for handling 404's,
-- and segment encoders for backend and frontend routes.
mkFullRouteEncoder
  :: (GCompare br, GCompare fr, GShow br, GShow fr, UniverseSome br, UniverseSome fr)
  => R (FullRoute br fr) -- ^ 404 handler
  -> (forall a. br a -> SegmentResult (Either Text) (Either Text) a) -- ^ How to encode a single backend route segment
  -> (forall a. fr a -> SegmentResult (Either Text) (Either Text) a) -- ^ How to encode a single frontend route segment
  -> Encoder (Either Text) Identity (R (FullRoute br fr)) PageName
mkFullRouteEncoder missing backendSegment frontendSegment = handleEncoder (const missing) $
  pathComponentEncoder $ \case
    FullRoute_Backend backendRoute -> backendSegment backendRoute
    FullRoute_Frontend obeliskRoute -> obeliskRouteSegment obeliskRoute frontendSegment

-- | A type which can represent Obelisk-specific resource routes, in addition to application specific routes which serve your
-- frontend.
data ObeliskRoute :: (* -> *) -> * -> * where
  -- We need to have the `f a` as an argument here, because otherwise we have no way to specifically check for overlap between us and the given encoder
  ObeliskRoute_App :: f a -> ObeliskRoute f a
  ObeliskRoute_Resource :: ResourceRoute a -> ObeliskRoute f a

instance UniverseSome f => UniverseSome (ObeliskRoute f) where
  universeSome = concat
    [ (\(Some x) -> Some (ObeliskRoute_App x)) <$> universe
    , (\(Some x) -> Some (ObeliskRoute_Resource x)) <$> (universe @(Some ResourceRoute))
    ]

instance GEq f => GEq (ObeliskRoute f) where
  geq (ObeliskRoute_App x) (ObeliskRoute_App y) = geq x y
  geq (ObeliskRoute_Resource x) (ObeliskRoute_Resource y) = geq x y
  geq _ _ = Nothing

instance GCompare f => GCompare (ObeliskRoute f) where
  gcompare (ObeliskRoute_App x) (ObeliskRoute_App y) = gcompare x y
  gcompare (ObeliskRoute_Resource x) (ObeliskRoute_Resource y) = gcompare x y
  gcompare (ObeliskRoute_App _) (ObeliskRoute_Resource _) = GLT
  gcompare (ObeliskRoute_Resource _) (ObeliskRoute_App _) = GGT

-- | A type representing the various resource routes served by Obelisk. These can in principle map to any physical routes you want,
-- but sane defaults are provided by 'resourceRouteSegment'
data ResourceRoute :: * -> * where
  ResourceRoute_Static :: ResourceRoute [Text] -- This [Text] represents the *path in our static files directory*, not necessarily the URL path that the asset gets served at (although that will often be "/static/this/text/thing")
  ResourceRoute_Ghcjs :: ResourceRoute [Text]
  ResourceRoute_JSaddleWarp :: ResourceRoute (R JSaddleWarpRoute)
  ResourceRoute_Version :: ResourceRoute ()

-- | If there are no additional backend routes in your app (i.e. ObeliskRoute gives you all the routes you need),
-- this constructs a suitable 'Encoder' to use for encoding routes to 'PageName's. If you do have additional backend routes,
-- you'll want to use 'pathComponentEncoder' yourself, applied to a function that will likely use obeliskRouteSegment in order to
-- handle the ObeliskRoute case (i.e. Obelisk resource routes and app frontend routes).
obeliskRouteEncoder :: forall check parse appRoute.
     ( Universe (Some (ObeliskRoute appRoute))
     , GCompare (ObeliskRoute appRoute)
     , GShow appRoute
     , MonadError Text check
     , check ~ parse --TODO: Get rid of this
     )
  => (forall a. appRoute a -> SegmentResult check parse a)
  -> Encoder check parse (R (ObeliskRoute appRoute)) PageName
obeliskRouteEncoder appRouteSegment = pathComponentEncoder $ \r ->
  obeliskRouteSegment r appRouteSegment

-- | From a function which explains how app-specific frontend routes translate into segments, produce a function which does the
-- same for ObeliskRoute. This uses the given function for the 'ObeliskRoute_App' case, and 'resourceRouteSegment' for the
-- 'ObeliskRoute_Resource' case.
obeliskRouteSegment :: forall check parse appRoute a.
     (MonadError Text check, MonadError Text parse)
  => ObeliskRoute appRoute a
  -> (forall b. appRoute b -> SegmentResult check parse b)
  -> SegmentResult check parse a
obeliskRouteSegment r appRouteSegment = case r of
  ObeliskRoute_App appRoute -> appRouteSegment appRoute
  ObeliskRoute_Resource resourceRoute -> resourceRouteSegment resourceRoute

-- | A function which gives a sane default for how to encode Obelisk resource routes. It's given in this form, because it will
-- be combined with other such segment encoders before 'pathComponentEncoder' turns it into a proper 'Encoder'.
resourceRouteSegment :: (MonadError Text check, MonadError Text parse) => ResourceRoute a -> SegmentResult check parse a
resourceRouteSegment = \case
  ResourceRoute_Static -> PathSegment "static" pathOnlyEncoderIgnoringQuery
  ResourceRoute_Ghcjs -> PathSegment "ghcjs" pathOnlyEncoder
  ResourceRoute_JSaddleWarp -> PathSegment "jsaddle" jsaddleWarpRouteEncoder
  ResourceRoute_Version -> PathSegment "version" $ unitEncoder mempty

data JSaddleWarpRoute :: * -> * where
  JSaddleWarpRoute_JavaScript :: JSaddleWarpRoute ()
  JSaddleWarpRoute_WebSocket :: JSaddleWarpRoute ()
  JSaddleWarpRoute_Sync :: JSaddleWarpRoute [Text]

jsaddleWarpRouteEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (R JSaddleWarpRoute) PageName
jsaddleWarpRouteEncoder = pathComponentEncoder $ \case
  JSaddleWarpRoute_JavaScript -> PathSegment "jsaddle.js" $ unitEncoder mempty
  JSaddleWarpRoute_WebSocket ->  PathEnd $ unitEncoder mempty
  JSaddleWarpRoute_Sync -> PathSegment "sync" pathOnlyEncoder

instance GShow appRoute => GShow (ObeliskRoute appRoute) where
  gshowsPrec prec = \case
    ObeliskRoute_App appRoute -> showParen (prec > 10) $
      showString "ObeliskRoute_App " . gshowsPrec 11 appRoute
    ObeliskRoute_Resource appRoute -> showParen (prec > 10) $
      showString "ObeliskRoute_Resource " . gshowsPrec 11 appRoute

data IndexOnlyRoute :: * -> * where
  IndexOnlyRoute :: IndexOnlyRoute ()

indexOnlyRouteSegment :: (Applicative check, MonadError Text parse) => IndexOnlyRoute a -> SegmentResult check parse a
indexOnlyRouteSegment = \case
  IndexOnlyRoute -> PathEnd $ unitEncoder mempty

indexOnlyRouteEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (R IndexOnlyRoute) PageName
indexOnlyRouteEncoder = pathComponentEncoder indexOnlyRouteSegment

someSumEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Some (Sum a b)) (Either (Some a) (Some b))
someSumEncoder = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = \(Some t) -> case t of
      InL l -> Left $ Some l
      InR r -> Right $ Some r
  , _encoderImpl_decode = pure . \case
      Left (Some l) -> Some (InL l)
      Right (Some r) -> Some (InR r)
  }

data Void1 :: * -> * where {}

instance UniverseSome Void1 where
  universeSome = []
instance FiniteSome Void1

void1Encoder :: (Applicative check, MonadError Text parse) => Encoder check parse (Some Void1) a
void1Encoder = Encoder $ pure $ EncoderImpl
  { _encoderImpl_encode = foldSome $ \case
  , _encoderImpl_decode = \_ -> throwError "void1Encoder: can't decode anything"
  }

instance GShow Void1 where
  gshowsPrec _ = \case {}

-- | Encode a 'PathQuery' as 'Text'
pathQueryEncoder :: (Applicative check, Applicative parse) => Encoder check parse PathQuery Text
pathQueryEncoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = \(k, v) -> T.pack $ k <> v
  , _encoderImpl_decode = \r ->
      pure $ bimap T.unpack T.unpack $ T.breakOn "?" r
  }

-- | Given a backend route and a checked route encoder, render the route (path
-- and query string). See 'checkEncoder' for how to produce a checked encoder.
renderBackendRoute
  :: forall br a.
     Encoder Identity Identity (R (FullRoute br a)) PageName
  -> R br
  -> Text
renderBackendRoute enc = renderObeliskRoute enc . hoistR FullRoute_Backend

-- | Renders a frontend route with the supplied checked encoder
renderFrontendRoute
  :: forall a fr.
     Encoder Identity Identity (R (FullRoute a fr)) PageName
  -> R fr
  -> Text
renderFrontendRoute enc = renderObeliskRoute enc . hoistR (FullRoute_Frontend . ObeliskRoute_App)

-- | Renders a route of the form typically found in an Obelisk project
renderObeliskRoute
  :: forall a b.
     Encoder Identity Identity (R (FullRoute a b)) PageName
  -> R (FullRoute a b)
  -> Text
renderObeliskRoute e r =
  let enc :: Encoder Identity (Either Text) (R (FullRoute a b)) Text
      enc = (pathQueryEncoder . pageNameEncoder . generalizeIdentity e)
  in encode enc r

-- | As per the 'unsafeTshowEncoder' but does not use the 'Text' type.
--
-- WARNING: Just like 'unsafeTshowEncoder' this is only safe if the Show and Read
-- instances for 'a' are inverses of each other
--
-- Instances must be able to satisfy the following property for this 'Encoder' to be safe:
--
-- @
-- forall a. reads (show a) === [(a, "")]
-- @
--
unsafeShowEncoder :: (MonadError Text parse, Read a, Show a, Applicative check) => Encoder check parse a PageName
unsafeShowEncoder = singlePathSegmentEncoder . unsafeTshowEncoder

-- | This 'Encoder' does not properly indicate that its use may be unsafe and is being renamed to 'unsafeShowEncoder'
readShowEncoder :: (MonadError Text parse, Read a, Show a, Applicative check) => Encoder check parse a PageName
readShowEncoder = unsafeShowEncoder
{-# DEPRECATED readShowEncoder "This function has been renamed to 'unsafeShowEncoder'. 'readShowEncoder' will be removed in a future release" #-}

integralEncoder :: (MonadError Text parse, Applicative check, Integral a) => Encoder check parse a Integer
integralEncoder = reviewEncoder Numeric.Lens.integral

pathSegmentEncoder :: (MonadError Text parse, Applicative check, Cons as as a a) =>
  Encoder check parse (a, (as, b)) (as, b)
pathSegmentEncoder = first (reviewEncoder _Cons) . disassociate

newtype Decoder check parse b a = Decoder { toEncoder :: Encoder check parse a b }

dmapEncoder :: forall check parse k k' v.
   ( Monad check
   , MonadError Text parse
   , Universe (Some k')
   , Ord k
   , GCompare k'
   , GShow k'
   )
  => Encoder check parse (Some k') k
  -> (forall v'. k' v' -> Encoder check parse v' v)
  -> Encoder check parse (DMap k' Identity) (Map k v)
dmapEncoder keyEncoder' valueEncoderFor = unsafeEncoder $ do
  keyEncoder :: Encoder Identity parse (Some k') k <- checkEncoder keyEncoder'
  valueDecoders :: DMap k' (Decoder Identity parse v) <- fmap DMap.fromList . forM universe $ \(Some (k' :: k' t)) -> do
    ve :: Encoder Identity parse t v <- checkEncoder (valueEncoderFor k')
    return $ (k' :: k' t) :=> (Decoder ve :: Decoder Identity parse v t)
  let keyError k = "dmapEncoder: key `" <> k <> "' was missing from the Universe instance for its type."
  return $ EncoderImpl
    { _encoderImpl_encode = \dm -> Map.fromList $ do
        ((k' :: k' t) :=> Identity v') <- DMap.toList dm
        return ( encode keyEncoder (Some k')
               , encode (toEncoder (DMap.findWithDefault (error . keyError $ gshow k') k' valueDecoders)) v'
               )
    , _encoderImpl_decode = \m -> fmap DMap.fromList . forM (Map.toList m) $ \(k,v) -> do
          tryDecode keyEncoder k >>= \case
            Some (k' :: k' t) -> case DMap.lookup k' valueDecoders of
              Nothing -> throwError . T.pack . keyError $ gshow k'
              Just (Decoder e) -> do
                v' <- tryDecode e v
                return (k' :=> Identity v')
    }

fieldMapEncoder :: forall check parse r.
   ( Applicative check
   , MonadError Text parse
   , HasFields r
   , Universe (Some (Field r))
   , GShow (Field r)
   , GCompare (Field r)
   )
  => Encoder check parse r (DMap (Field r) Identity)
fieldMapEncoder = unsafeEncoder $ do
  pure $ EncoderImpl
    { _encoderImpl_encode = \r -> DMap.fromList [ f :=> Identity (indexField r f) | Some f <- universe ]
    , _encoderImpl_decode = \dm -> tabulateFieldsA $ \f -> do
      case DMap.lookup f dm of
        Nothing -> throwError $ "fieldMapEncoder: Couldn't find key for `" <> T.pack (gshow f) <> "' in DMap."
        Just (Identity v) -> return v
    }

pathFieldEncoder :: forall a p check parse . (HasFields a, Monad check, MonadError Text parse, GCompare (Field a)) => (forall x. Field a x -> Encoder check parse x p) -> Encoder check parse (a, [p]) [p]
pathFieldEncoder fieldEncoder = unsafeEncoder $ do
  fieldEncoderPureMap :: DMap.DMap (Field a) (Decoder Identity parse p) <- getAp $ getConst $ tabulateFieldsA @a $ \f -> Const (Ap $ fmap (DMap.singleton f . Decoder) $ (checkEncoder @Identity) $ fieldEncoder f)
  let fieldEncoderPure :: forall x. Field a x -> Encoder Identity parse x p
      fieldEncoderPure f = toEncoder (DMap.findWithDefault (error "bad") f fieldEncoderPureMap)
  pure $ EncoderImpl
    { _encoderImpl_encode = \(x, rest) -> execWriter $ do
      _ <- traverseWithField (\f x_i -> tell (pure $ encode (fieldEncoderPure f) x_i) $> x_i) x
      tell rest
    , _encoderImpl_decode = State.runStateT $ tabulateFieldsA $ \f -> State.get >>= \case
      [] -> throwError $ T.pack "not enough path components"
      p:ps -> do
        State.put ps
        lift $ tryDecode (fieldEncoderPure f) p
    }

-- | Use ToJSON/FromJSON to encode to Text. The correctness of this encoder is dependent on the encoding being injective and round-tripping correctly.
jsonEncoder :: forall check parse r.
  ( ToJSON r
  , FromJSON r
  , Applicative check
  , MonadError Text parse
  )
  => Encoder check parse r Text
jsonEncoder = unsafeEncoder $ do
  pure $ EncoderImpl
    { _encoderImpl_encode = \r -> T.decodeUtf8 . BSL.toStrict $ Aeson.encode r
    , _encoderImpl_decode = \t -> case Aeson.eitherDecodeStrict $ T.encodeUtf8 t of
        Left err -> throwError ("jsonEncoder: " <> T.pack err)
        Right x -> return x
    }

utf8Encoder :: (Applicative check, MonadError Text parse) => Encoder check parse Text BS.ByteString
utf8Encoder = unsafeMkEncoder $ EncoderImpl
  { _encoderImpl_encode = T.encodeUtf8
  , _encoderImpl_decode = \bs -> case T.decodeUtf8' bs of
      Left err -> throwError $ "utf8Encoder: " <> tshow err
      Right x -> pure x
  }

-- Useful for app server integration.
-- p must not start with slashes
byteStringsToPageName :: BS.ByteString -> BS.ByteString -> PageName
byteStringsToPageName p q =
  let pageNameEncoder' :: Encoder Identity Identity PageName (String, String)
      pageNameEncoder' = bimap
        (unpackTextEncoder . pathSegmentsTextEncoder . listToNonEmptyEncoder)
        (unpackTextEncoder . queryParametersTextEncoder . toListMapEncoder)
  in decode pageNameEncoder' (T.unpack (T.decodeUtf8 p), T.unpack (T.decodeUtf8 q))

--TODO: decodeURIComponent as appropriate


{-# DEPRECATED isoEncoder "Instead of 'isoEncoder f', use 'viewEncoder f'" #-}
-- | Given a valid 'Iso' from lens, construct an 'Encoder'
isoEncoder :: (Applicative check, Applicative parse) => Iso' a b -> Encoder check parse a b
isoEncoder = viewEncoder

{-# DEPRECATED prismEncoder "Instead of 'prismEncoder f', use 'reviewEncoder f'" #-}
-- | An encoder that only works on the items available via the prism. An error will be thrown in the parse monad
-- if the prism doesn't match.
--
-- Note that a 'Prism' from @a@ to @b@ will produce an 'Encoder' from @b@ to @a@
-- (i.e. 'prismEncoder' is a contravariant functor from the category of prisms to the category of encoders),
-- just like 'review' produces a function @b -> a@. This is because 'Prism's extract values, in a way that might
-- fail, in their forward direction and inject values, in a way that cannot fail, in their reverse direction;
-- whereas 'Encoder's encode, which cannot fail, in their forward direction, and decode, which can fail, in their
-- reverse direction. In short @prismEncoder (f . g) = prismEncoder g . prismEncoder f@.
prismEncoder :: (Applicative check, MonadError Text parse) => Prism' b a -> Encoder check parse a b
prismEncoder = reviewEncoder


concat <$> mapM deriveRouteComponent
  [ ''ResourceRoute
  , ''JSaddleWarpRoute
  , ''IndexOnlyRoute
  ]

makePrisms ''ObeliskRoute
makePrisms ''FullRoute
deriveGEq ''Void1
deriveGCompare ''Void1

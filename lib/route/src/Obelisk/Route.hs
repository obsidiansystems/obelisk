{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
module Obelisk.Route
  ( R
  , pattern (:/)
  , PageName
  , Encoder (..) --TODO: unsafe
  , checkEncoder
  , ValidEncoder (..) --TODO: unsafe
  , mapSome
  , pathComponentEncoder
  , enumEncoder
  , enum1Encoder
  , endValidEncoder
  , pathOnlyValidEncoder
  , singletonListValidEncoder
  , unpackTextEncoder
  , prefixTextEncoder
  , intercalateTextEncoder
  , listToNonEmptyEncoder
  , prefixNonemptyTextEncoder
  , joinPairTextEncoder
  , toListMapEncoder
  , shadowEncoder
  , prismValidEncoder
  , rPrism
  , obeliskRouteEncoder
  , pageNameValidEncoder
  , catchValidEncoder
  , ObeliskRoute (..)
  , _ObeliskRoute_App
  , _ObeliskRoute_Resource
  , ResourceRoute (..)
  , JSaddleWarpRoute (..)
  , jsaddleWarpRouteEncoder
  , IndexOnlyRoute (..)
  , indexOnlyRouteComponentEncoder
  , indexOnlyRouteRestEncoder
  ) where

import Prelude hiding ((.), id)

import Control.Applicative
import Control.Category (Category (..))
import qualified Control.Categorical.Functor as Cat
import Control.Categorical.Bifunctor
import Control.Lens (Identity (..), Prism', makePrisms, itraverse, imap, prism, (^.), re, matching, (^?))
import Control.Monad.Except
import Data.Dependent.Sum (DSum (..), ShowTag (..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Foldable
import Data.GADT.Compare
import Data.GADT.Compare.TH
import Data.GADT.Show
import Data.GADT.Show.TH
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Some (Some)
import qualified Data.Some as Some
import Data.Text (Text)
import qualified Data.Text as T
import Data.Either.Validation (Validation (..))
import Data.Universe

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

type R f = DSum f Identity --TODO: Better name

--TODO: COMPLETE pragma
infixr 5 :/
pattern (:/) :: f a -> a -> R f
pattern a :/ b = a :=> Identity b

mapSome :: (forall a. f a -> g a) -> Some f -> Some g
mapSome f (Some.This a) = Some.This $ f a

--------------------------------------------------------------------------------
-- Encoder fundamentals
--------------------------------------------------------------------------------

newtype Encoder check parse decoded encoded = Encoder { unEncoder :: check (ValidEncoder parse decoded encoded) }

-- | Law:
-- forall p. _validEncoder_decode ve . _validEncoder_encode ve p == pure
-- Note that the reverse may not be the case: when parsing, a route may be canonicalized, and erroneous routes may be collapsed to a single 404 route.  However, as a consequence of the law, encode . decode must be idemponent.
data ValidEncoder parse decoded encoded = ValidEncoder
  { _validEncoder_decode :: !(encoded -> parse decoded) -- Can fail; can lose information; must always succeed on outputs of `_validEncoder_encode` and result in the original value
  , _validEncoder_encode :: !(decoded -> encoded) -- Must be injective
  }

checkEncoder :: Encoder check parse decoded encoded -> check (ValidEncoder parse decoded encoded)
checkEncoder = unEncoder

instance (Applicative check, Monad parse) => Category (Encoder check parse) where
  id = Encoder $ pure id
  Encoder f . Encoder g = Encoder $ liftA2 (.) f g

instance Monad parse => Category (ValidEncoder parse) where
  id = ValidEncoder
    { _validEncoder_decode = pure
    , _validEncoder_encode = id
    }
  f . g = ValidEncoder
    { _validEncoder_decode = _validEncoder_decode g <=< _validEncoder_decode f
    , _validEncoder_encode = _validEncoder_encode f . _validEncoder_encode g
    }

instance Monad parse => PFunctor (,) (ValidEncoder parse) (ValidEncoder parse) where
  first f = bimap f id
instance Monad parse => QFunctor (,) (ValidEncoder parse) (ValidEncoder parse) where
  second g = bimap id g
instance Monad parse => Bifunctor (,) (ValidEncoder parse) (ValidEncoder parse) (ValidEncoder parse) where
  bimap f g = ValidEncoder
    { _validEncoder_encode = bimap (_validEncoder_encode f) (_validEncoder_encode g)
    , _validEncoder_decode = \(a, b) -> liftA2 (,) (_validEncoder_decode f a) (_validEncoder_decode g b)
    }

instance (Applicative check, Monad parse) => PFunctor (,) (Encoder check parse) (Encoder check parse) where
  first f = bimap f id
instance (Applicative check, Monad parse) => QFunctor (,) (Encoder check parse) (Encoder check parse) where
  second g = bimap id g
instance (Applicative check, Monad parse) => Bifunctor (,) (Encoder check parse) (Encoder check parse) (Encoder check parse) where
  bimap f g = Encoder $ liftA2 bimap (checkEncoder f) (checkEncoder g)

instance (Traversable f, Monad check, Monad parse) => Cat.Functor f (Encoder check parse) (Encoder check parse) where
  fmap e = Encoder $ do
    ve <- checkEncoder e
    pure $ ValidEncoder
      { _validEncoder_encode = fmap $ _validEncoder_encode ve
      , _validEncoder_decode = traverse $ _validEncoder_decode ve
      }

--------------------------------------------------------------------------------
-- Specific instances of encoders
--------------------------------------------------------------------------------

-- | A path and query string, in which trailing slashes don't matter in the path
-- and duplicate query parameters are not allowed
type PageName = ([Text], Map Text Text)

newtype Flip f a b = Flip { unFlip :: f b a }

pathComponentEncoder :: forall check parse p. (Monad check, Monad parse) => (Encoder check parse (Some p) (Maybe Text)) -> (forall a. p a -> ValidEncoder parse a PageName) -> Encoder check parse (R p) PageName
pathComponentEncoder this rest = chainEncoder (lensEncoder (\(_, b) a -> (a, b)) Prelude.fst consValidEncoder) this rest

--NOTE: Naming convention in this module is to always talk about things in the *encoding* direction, never in the *decoding* direction

chainEncoder
  :: forall check parse p r b.
     ( Monad check
     , Monad parse
     )
  => ValidEncoder parse (b, r) r
  -> Encoder check parse (Some p) b
  -> (forall a. p a -> ValidEncoder parse a r)
  -> Encoder check parse (R p) r
chainEncoder cons this rest = Encoder $ do
  thisValid <- checkEncoder this
  pure $ ValidEncoder
    { _validEncoder_decode = \v -> do
        (here, following) <- _validEncoder_decode cons v
        Some.This r <- _validEncoder_decode thisValid here
        (r :/) <$> _validEncoder_decode (rest r) following
    , _validEncoder_encode = \(r :/ s) ->
        _validEncoder_encode cons (_validEncoder_encode thisValid $ Some.This r, _validEncoder_encode (rest r) s)
    }

--TODO: Do this in terms of a lens instead
lensEncoder :: Monad parse => (b -> [a] -> b) -> (b -> [a]) -> ValidEncoder parse (c, [a]) [a] -> ValidEncoder parse (c, b) b
lensEncoder set get g = ValidEncoder
  { _validEncoder_encode = \(ma, b) -> set b $ _validEncoder_encode g (ma, get b)
  , _validEncoder_decode = \b -> do
      (ma, la) <- _validEncoder_decode g $ get b
      pure (ma, set b la)
  }

consValidEncoder :: Applicative parse => ValidEncoder parse (Maybe a, [a]) [a] --TODO: Really shouldn't *always* have the [a], even in the Nothing case
consValidEncoder = ValidEncoder
  { _validEncoder_encode = \(h, t) -> maybeToList h <> t
  , _validEncoder_decode = pure . \case
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
  => Encoder check parse a c -- ^ Overlaps
  -> Encoder check parse b c -- ^ Gets overlapped
  -> Encoder check parse (Either a b) c
shadowEncoder f g = Encoder $ do
  vf <- checkEncoder f
  vg <- checkEncoder g
  let gCanParse c = catchError (Just <$> _validEncoder_decode vg c) (\_ -> pure Nothing)
  overlaps <- fmap catMaybes $ forM universe $ \a -> do
    let c = _validEncoder_encode vf a
    mb <- gCanParse c
    pure $ fmap (\b -> (a, b, c)) mb
  case overlaps of
    [] -> pure ()
    _ -> throwError $ "shadowEncoder: overlap detected: " <> T.unlines
      (flip fmap overlaps $ \(a, b, c) -> "first encoder encodes " <> tshow a <> " as " <> tshow c <> ", which second encoder decodes as " <> tshow b)
  pure $ ValidEncoder
    { _validEncoder_encode = \case
        Left a -> _validEncoder_encode vf a
        Right b -> _validEncoder_encode vg b
    , _validEncoder_decode = \c -> (Left <$> _validEncoder_decode vf c) `catchError` \_ -> Right <$> _validEncoder_decode vg c
    }

enum1Encoder
  :: ( Universe (Some p)
     , GShow p
     , GCompare p
     , Ord r
     , MonadError Text check
     , MonadError Text parse
     , Show r
     )
  => (forall a. p a -> r) -> Encoder check parse (Some p) r
enum1Encoder f = enumEncoder $ \(Some.This p) -> f p

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
    Success m -> pure $ ValidEncoder
      { _validEncoder_decode = \r -> case Map.lookup r m of
          Just a -> pure a
          Nothing -> throwError $ "enumEncoder: not recognized: " <> tshow r --TODO: Report this as a better type
      , _validEncoder_encode = f
      }

endValidEncoder :: (MonadError Text parse, Show r, Eq r) => r -> ValidEncoder parse () r
endValidEncoder expected = ValidEncoder
  { _validEncoder_decode = \obtained ->
      if obtained == expected
      then pure ()
      else throwError $ "endValidEncoder: expected " <> tshow expected <> ", got " <> tshow obtained
  , _validEncoder_encode = \_ -> expected
  }

pathOnlyValidEncoder :: (MonadError Text parse) => ValidEncoder parse [Text] PageName
pathOnlyValidEncoder = ValidEncoder
  { _validEncoder_decode = \(path, query) ->
      if query == mempty
      then pure path
      else throwError "pathOnlyValidEncoder: query was provided"
  , _validEncoder_encode = \path -> (path, mempty)
  }

singletonListValidEncoder :: (MonadError Text parse) => ValidEncoder parse a [a]
singletonListValidEncoder = ValidEncoder
  { _validEncoder_decode = \case
      [a] -> pure a
      l -> throwError $ "singletonListValidEncoder: expected one item, got " <> tshow (length l)
  , _validEncoder_encode = (:[])
  }

--TODO: To know this is reversible, we must know that the separator isn't included anywhere in the input text
intercalateTextEncoder :: (MonadError Text check, Applicative parse) => Text -> Encoder check parse (NonEmpty Text) Text
intercalateTextEncoder = Encoder . \case
  "" -> throwError "splitOnTextEncoder: empty split string"
  separator -> pure $ ValidEncoder
    { _validEncoder_encode = T.intercalate separator . toList
    , _validEncoder_decode = \r -> pure $ case T.splitOn separator r of
        [] -> error "intercalateTextEncoder: Data.Text.splitOn should never return an empty list"
        h : t -> h :| t
    }

listToNonEmptyEncoder :: (Applicative check, Applicative parse, Monoid a, Eq a) => Encoder check parse [a] (NonEmpty a)
listToNonEmptyEncoder = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = \case
      [] -> mempty :| []
      h : t -> h :| t
  , _validEncoder_decode = \(h :| t) -> pure $
      if h == mempty
      then []
      else h : t
  }

prefixTextEncoder :: (Applicative check, MonadError Text parse) => Text -> Encoder check parse Text Text
prefixTextEncoder p = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = mappend p
  , _validEncoder_decode = \v -> case T.stripPrefix p v of
      Nothing -> throwError $ "prefixTextEncoder: wrong prefix; expected " <> tshow p <> ", got " <> tshow (T.take (T.length p) v)
      Just stripped -> pure stripped
  }

prefixNonemptyTextEncoder :: (Applicative check, MonadError Text parse) => Text -> Encoder check parse Text Text
prefixNonemptyTextEncoder p = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = \case
      "" -> ""
      v -> p <> v
  , _validEncoder_decode = \case
      "" -> pure ""
      v -> case T.stripPrefix p v of
        Nothing -> throwError $ "prefixTextEncoder: wrong prefix; expected " <> tshow p
        Just stripped -> pure stripped
  }

unpackTextEncoder :: (Applicative check, Applicative parse) => Encoder check parse Text String
unpackTextEncoder = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = T.unpack
  , _validEncoder_decode = pure . T.pack
  }

toListMapEncoder :: (Applicative check, Applicative parse, Ord k) => Encoder check parse (Map k v) [(k, v)]
toListMapEncoder = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = Map.toList
  , _validEncoder_decode = pure . Map.fromList --TODO: Should we be stricter about repeated keys?
  }

joinPairTextEncoder :: (MonadError Text check, MonadError Text parse, Applicative parse) => Text -> Encoder check parse (Text, Text) Text
joinPairTextEncoder = Encoder . \case
  "" -> throwError "joinPairTextEncoder: empty separator"
  separator -> pure $ ValidEncoder
    { _validEncoder_encode = \(k, v) -> k <> separator <> v
    , _validEncoder_decode = \r ->
        let (kt, vt) = T.breakOn separator r
        in case vt of
          -- The separator was not found
          "" -> throwError $ "joinPairTextEncoder: separator not found; expected " <> tshow separator
          _ -> return (kt, T.drop (T.length separator) vt)
    }

--TODO: Rewrite this by composing the given prism with a lens on the first element of the DSum
-- Or, more likely, the user can compose it themselves
rPrism :: forall f g. (forall a. Prism' (f a) (g a)) -> Prism' (R f) (R g)
rPrism p = prism (\(g :/ x) -> g ^. re p :/ x) (\(f :/ x) -> bimap (:/ x) (:/ x) $ matching p f)

-- An encoder that only works on the items available via the prism
prismValidEncoder :: MonadError Text parse => Prism' b a -> ValidEncoder parse a b
prismValidEncoder p = ValidEncoder
  { _validEncoder_encode = (^. re p)
  , _validEncoder_decode = \r -> case r ^? p of
      Just a -> pure a
      Nothing -> throwError "prismValidEncoder: value is not present in the prism"
  }


-- | Encode a PageName into a path and query string, suitable for use in the
-- 'URI' type
pageNameValidEncoder :: MonadError Text parse => ValidEncoder parse PageName (String, String)
pageNameValidEncoder = ve
  where Right ve = checkEncoder $ bimap
          (unpackTextEncoder . prefixTextEncoder "/" . intercalateTextEncoder "/" . listToNonEmptyEncoder)
          (unpackTextEncoder . prefixNonemptyTextEncoder "?" . intercalateTextEncoder "&" . listToNonEmptyEncoder . Cat.fmap (joinPairTextEncoder "=") . toListMapEncoder)

catchValidEncoder :: (e -> a) -> ValidEncoder (Either e) a b -> ValidEncoder Identity a b
catchValidEncoder recover ve = ve
  { _validEncoder_decode = \a -> pure $ case _validEncoder_decode ve a of
      Right r -> r
      Left err -> recover err
  }

--------------------------------------------------------------------------------
-- Actual obelisk route info
--------------------------------------------------------------------------------

data ObeliskRoute :: (* -> *) -> * -> * where
  -- We need to have the `f a` as an argument here, because otherwise we have no way to specifically check for overlap between us and the given encoder
  ObeliskRoute_App :: f a -> ObeliskRoute f a
  ObeliskRoute_Resource :: ResourceRoute a -> ObeliskRoute f a

data ResourceRoute :: * -> * where
  ResourceRoute_Static :: ResourceRoute [Text] -- This [Text] represents the *path in our static files directory*, not necessarily the URL path that the asset gets served at (although that will often be "/static/this/text/thing")
  ResourceRoute_Ghcjs :: ResourceRoute [Text]
  ResourceRoute_JSaddleWarp :: ResourceRoute (R JSaddleWarpRoute)

--TODO: Generate this
instance Universe (Some ResourceRoute) where
  universe =
    [ Some.This ResourceRoute_Static
    , Some.This ResourceRoute_Ghcjs
    , Some.This ResourceRoute_JSaddleWarp
    ]

--TODO: Figure out a way to check this
obeliskComponentEncoder :: (Applicative check, Applicative parse) => Encoder check parse (Some (ObeliskRoute f)) (Either (Some ResourceRoute) (Some f))
obeliskComponentEncoder = Encoder $ pure $ ValidEncoder
  { _validEncoder_encode = \(Some.This o) -> case o of
      ObeliskRoute_App r -> Right $ Some.This r
      ObeliskRoute_Resource r -> Left $ Some.This r
  , _validEncoder_decode = pure . \case
      Right (Some.This r) -> Some.This $ ObeliskRoute_App r
      Left (Some.This r) -> Some.This $ ObeliskRoute_Resource r
  }

newtype ValidEncoderFunc parse p r = ValidEncoderFunc { runValidEncoderFunc :: forall a. p a -> ValidEncoder parse a r }

checkSomeUniverseEncoder
  :: forall check parse p r.
     ( Universe (Some p)
     , GCompare p
     , Monad check
     )
  => (forall a. p a -> Encoder check parse a r)
  -> check (ValidEncoderFunc parse p r)
checkSomeUniverseEncoder f = do
  validEncoders :: DMap p (Flip (ValidEncoder parse) r) <- DMap.fromList <$> traverse (\(Some.This p) -> (p :=>) . Flip <$> checkEncoder (f p)) universe
  pure $ ValidEncoderFunc $ \p -> unFlip $ DMap.findWithDefault (error "checkSomeUniverseEncoder: ValidEncoder not found (should be impossible)") p validEncoders

obeliskRouteEncoder
  :: forall check parse appRoute.
     ( Universe (Some appRoute)
     , GCompare appRoute
     , GShow appRoute
     , MonadError Text check
     , MonadError Text parse
     , check ~ parse --TODO: Get rid of this
     )
  => (Encoder check parse (Some appRoute) (Maybe Text))
  -> (forall a. appRoute a -> Encoder check parse a PageName)
  -> Encoder check parse (R (ObeliskRoute appRoute)) PageName
obeliskRouteEncoder appComponentEncoder appRouteEncoder = Encoder $ do
  let componentEncoder = (resourceComponentEncoder `shadowEncoder` appComponentEncoder) . obeliskComponentEncoder
  appRouteValidEncoder <- checkSomeUniverseEncoder appRouteEncoder
  resourceRouteValidEncoder <- checkSomeUniverseEncoder resourceRouteEncoder
  checkEncoder $ pathComponentEncoder componentEncoder $ \case
    ObeliskRoute_App appRoute -> runValidEncoderFunc appRouteValidEncoder appRoute
    ObeliskRoute_Resource resRoute -> runValidEncoderFunc resourceRouteValidEncoder resRoute

resourceComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some ResourceRoute) (Maybe Text)
resourceComponentEncoder = enum1Encoder $ \case
  ResourceRoute_Static -> Just "static"
  ResourceRoute_Ghcjs -> Just "ghcjs"
  ResourceRoute_JSaddleWarp -> Just "jsaddle"

resourceRouteEncoder :: (MonadError Text check, MonadError Text parse) => ResourceRoute a -> Encoder check parse a PageName
resourceRouteEncoder = \case
  ResourceRoute_Static -> Encoder $ pure pathOnlyValidEncoder
  ResourceRoute_Ghcjs -> Encoder $ pure pathOnlyValidEncoder
  ResourceRoute_JSaddleWarp -> jsaddleWarpRouteEncoder

data JSaddleWarpRoute :: * -> * where
  JSaddleWarpRoute_JavaScript :: JSaddleWarpRoute ()
  JSaddleWarpRoute_WebSocket :: JSaddleWarpRoute ()
  JSaddleWarpRoute_Sync :: JSaddleWarpRoute [Text]

instance Universe (Some JSaddleWarpRoute) where
  universe =
    [ Some.This JSaddleWarpRoute_JavaScript
    , Some.This JSaddleWarpRoute_WebSocket
    , Some.This JSaddleWarpRoute_Sync
    ]

jsaddleWarpRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some JSaddleWarpRoute) (Maybe Text)
jsaddleWarpRouteComponentEncoder = enum1Encoder $ \case
  JSaddleWarpRoute_JavaScript -> Just "jsaddle.js"
  JSaddleWarpRoute_WebSocket -> Nothing
  JSaddleWarpRoute_Sync -> Just "sync"

jsaddleWarpRouteEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (R JSaddleWarpRoute) PageName
jsaddleWarpRouteEncoder = pathComponentEncoder jsaddleWarpRouteComponentEncoder $ \case
  JSaddleWarpRoute_JavaScript -> endValidEncoder mempty
  JSaddleWarpRoute_WebSocket -> endValidEncoder mempty
  JSaddleWarpRoute_Sync -> pathOnlyValidEncoder

instance ShowTag appRoute Identity => ShowTag (ObeliskRoute appRoute) Identity where
  showTaggedPrec = \case
    ObeliskRoute_App a -> showTaggedPrec a
    ObeliskRoute_Resource r -> showTaggedPrec r

instance ShowTag ResourceRoute Identity where
  showTaggedPrec = \case
    ResourceRoute_Static -> showsPrec
    ResourceRoute_Ghcjs -> showsPrec
    ResourceRoute_JSaddleWarp -> showsPrec

instance ShowTag JSaddleWarpRoute Identity where
  showTaggedPrec = \case
    JSaddleWarpRoute_JavaScript -> showsPrec
    JSaddleWarpRoute_WebSocket -> showsPrec
    JSaddleWarpRoute_Sync -> showsPrec

instance Universe (Some appRoute) => Universe (Some (ObeliskRoute appRoute)) where
  universe = mconcat
    [ mapSome ObeliskRoute_App <$> universe
    , mapSome ObeliskRoute_Resource <$> universe
    ]

instance GShow appRoute => GShow (ObeliskRoute appRoute) where
  gshowsPrec prec = \case
    ObeliskRoute_App appRoute -> showParen (prec > 10) $
      showString "ObeliskRoute_App " . gshowsPrec 11 appRoute
    ObeliskRoute_Resource appRoute -> showParen (prec > 10) $
      showString "ObeliskRoute_Resource " . gshowsPrec 11 appRoute


data IndexOnlyRoute :: * -> * where
  IndexOnlyRoute :: IndexOnlyRoute ()

instance Universe (Some IndexOnlyRoute) where
  universe = [Some.This IndexOnlyRoute]

indexOnlyRouteComponentEncoder :: (MonadError Text check, MonadError Text parse) => Encoder check parse (Some IndexOnlyRoute) (Maybe Text)
indexOnlyRouteComponentEncoder = enum1Encoder $ \case
  IndexOnlyRoute -> Nothing

indexOnlyRouteRestEncoder :: (Applicative check, MonadError Text parse) => IndexOnlyRoute a -> Encoder check parse a PageName
indexOnlyRouteRestEncoder = \case
  IndexOnlyRoute -> Encoder $ pure $ endValidEncoder mempty --TODO: Allow anything to parse

instance ShowTag IndexOnlyRoute Identity where
  showTaggedPrec = \case
    IndexOnlyRoute -> showsPrec

makePrisms ''ObeliskRoute
--TODO: Prevent deriveGShow from creating this warning:
-- Defined but not used: ‘p’
deriveGShow ''ResourceRoute
deriveGEq ''ResourceRoute
deriveGCompare ''ResourceRoute
deriveGShow ''JSaddleWarpRoute
deriveGEq ''JSaddleWarpRoute
deriveGCompare ''JSaddleWarpRoute
deriveGShow ''IndexOnlyRoute
deriveGEq ''IndexOnlyRoute
deriveGCompare ''IndexOnlyRoute

--TODO: decodeURIComponent as appropriate

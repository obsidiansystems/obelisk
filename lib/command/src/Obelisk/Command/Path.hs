{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An extremely low-ambition version of an opaque 'Path' library.
-- Libraries exist with much stronger guarantees but few (if any) of them
-- capture the idea of "canonical" paths in the types, which is critical
-- to many algorithms. Also they "buy-in" necessary for these approaches
-- is very high. This module provides the a minimal amount of type-level
-- tagging to allow the programmer to keep tabs on what's going on with
-- paths while also adding very little friction to interact with
-- 'FilePath'-based standard tools.
module Obelisk.Command.Path where

import Control.Applicative ((<|>))
import Control.Exception (IOException, catch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bifoldable (bifoldr1)
import Data.Bifunctor (bimap)
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Traversable (for)
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import Data.Coerce (Coercible, coerce)

-- | A newtype over 'FilePath' with a phantom type parameter to allow for various tagging mechanisms.
newtype Path t = Path { unPath :: FilePath } deriving (Eq, Ord)
instance Show (Path t) where
  show (Path p) = show p

-- | A 'Path' tag indicating that the path is canonical
data Canonical

-- | A 'Path' tag indicating that the path is absolute
data Absolute

-- | A 'Path' tag indicating that the path is relative
data Relative


type IsPath path = Coercible path FilePath

infixr 5 </>
(</>) :: (IsPath path1, IsPath path2) => path1 -> path2 -> FilePath
a </> b = coerce a FilePath.</> coerce b

rel :: FilePath -> Path Relative
rel = coerce

canonicalizePath :: (MonadIO m, IsPath path) => path -> m (Path Canonical)
canonicalizePath p = liftIO $ Path <$> Directory.canonicalizePath (coerce p)

makeAbsolute :: (MonadIO m, IsPath path) => path -> m (Path Absolute)
makeAbsolute p = liftIO $ Path <$> Directory.makeAbsolute (coerce p)

listDirectory :: (MonadIO m, IsPath path) => path -> m [Path Relative]
listDirectory p = liftIO $ coerce <$> Directory.listDirectory (coerce p)

splitDirectories :: IsPath path => path -> [String]
splitDirectories p = FilePath.splitDirectories (coerce p)

splitPath :: IsPath path => path -> [String]
splitPath p = FilePath.splitPath (coerce p)

createDirectoryIfMissing :: (MonadIO m, IsPath path) => Bool -> path -> m ()
createDirectoryIfMissing makeParents p = liftIO $ Directory.createDirectoryIfMissing makeParents (coerce p)

doesDirectoryExist :: (MonadIO m, IsPath path) => path -> m Bool
doesDirectoryExist p = liftIO $ Directory.doesDirectoryExist (coerce p)

doesFileExist :: (MonadIO m, IsPath path) => path -> m Bool
doesFileExist p = liftIO $ Directory.doesFileExist (coerce p)

takeDirectory :: Path t -> Path t
takeDirectory (Path p) = Path $ FilePath.takeDirectory p

takeFileName :: IsPath path => path -> Path Relative
takeFileName p = Path $ FilePath.takeFileName (coerce p)

takeBaseName :: IsPath path => path -> String
takeBaseName p = FilePath.takeBaseName (coerce p)

takeExtension :: IsPath path => path -> String
takeExtension p = FilePath.takeExtension (coerce p)

makeRelative :: (IsPath path1, IsPath path2) => path1 -> path2 -> FilePath
makeRelative a b = FilePath.makeRelative (coerce a) (coerce b)

data FindMatch a = FindMatch_SkipTree | FindMatch_SearchTree | FindMatch_Match a

-- | Like @find@ utility but in Haskell. Note that it follows symlinks!
findFiles'
  :: forall a path m. (MonadIO m, Monoid a, IsPath path)
  => (FilePath -> Path Canonical -> m (FindMatch a)) -- ^ Decide how to handle an encountered path.
  -> path -- ^ Root path to start searching
  -> m a -- ^ Matches
findFiles' match p' = go mempty (coerce p')
  where
    go :: Set (Path Canonical) -> FilePath -> m a
    go visited path = do
      canonicalPath <- canonicalizePath path
      if canonicalPath `Set.member` visited
        then pure mempty
        else match path canonicalPath >>= \case
          FindMatch_SkipTree -> pure mempty
          FindMatch_Match x -> pure x
          FindMatch_SearchTree -> do
            contents <- liftIO $ listDirectory canonicalPath `catch` \(_ :: IOException) -> pure []
            let visited' = Set.insert canonicalPath visited
            fmap fold $ for contents $ \p -> go visited' (path </> p)

-- | Like 'zipWith' but pads with a padding value instead of stopping on the shortest list.
zipDefaultWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipDefaultWith _da _db _f []     []     = []
zipDefaultWith  da  db  f (a:as) []     = f  a db : zipDefaultWith da db f as []
zipDefaultWith  da  db  f []     (b:bs) = f da  b : zipDefaultWith da db f [] bs
zipDefaultWith  da  db  f (a:as) (b:bs) = f  a  b : zipDefaultWith da db f as bs

-- | Makes the first absolute path relative to the second absolute path.
--
-- Both input paths MUST be canonical.
--
-- Unlike 'makeRelative' this does not merely strip prefixes. It will introduce
-- enough @..@ paths to make the resulting path truly relative in virtually every
-- case. The only exception is on Windows when the two paths are on different
-- drives. In this case the resulting path may be absolute.
relativeTo :: Path Canonical -> Path Canonical -> FilePath
relativeTo dir base
  = bifoldr1 (</>)
  $ bimap (collapse . (".." <$) . catMaybes) (collapse . catMaybes)
  $ unzip
  $ dropWhile (uncurry (==))
  $ zipDefaultWith Nothing Nothing (,)
    (map Just $ splitDirectories base)
    (map Just $ splitDirectories dir)
  where collapse = foldr (FilePath.</>) ""


-- | Describe a set of 'FilePath's as a tree to facilitate merging them in a convenient way.
data PathTree a = PathTree_Node
  (Maybe a) -- An optional leaf at this point in the tree
  (Map FilePath (PathTree a)) -- Branches to deeper leaves
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

emptyPathTree :: PathTree a
emptyPathTree = PathTree_Node Nothing mempty

-- | 2D ASCII drawing of a 'PathTree'. Adapted from Data.Tree.draw.
drawPathTree :: (a -> Text) -> PathTree a -> Text
drawPathTree showA (PathTree_Node _ ts0) = T.intercalate "\n" $ goForest (Map.toList ts0)
  where
    annotated ma = maybe id (\a b -> b <> " [" <> showA a <> "]") ma . T.pack
    goTree (fp, PathTree_Node ma forest) = annotated ma fp : goForest (Map.toList forest)
    goForest [] = []
    goForest [tree] = shift "└─ " "   " (goTree tree)
    goForest (tree:forest) = shift "├─ " "│  " (goTree tree) <> goForest forest
    shift first other = zipWith (<>) (first : repeat other)

-- | Traverses a 'PathTree' and folds all leaves matching a given predicate.
foldPathTreeFor
  :: forall m a b. (Applicative m, Monoid b)
  => (a -> Bool)
  -> FilePath
  -> PathTree a
  -> (FilePath -> PathTree a -> m b) -- ^ Decide how to collect this node given its path and its children
  -> m b
foldPathTreeFor predicate parent children f = case children of
  PathTree_Node (Just x) children' | predicate x -> f parent (PathTree_Node Nothing children')
  PathTree_Node _ children' -> fmap fold $ flip Map.traverseWithKey children' $ \k children'' ->
    foldPathTreeFor predicate (parent </> k) children'' f

-- | Convert a 'FilePath' into a 'PathTree'.
pathToTree :: IsPath path => a -> path -> PathTree a
pathToTree a p = go $ splitDirectories p
  where
    go [] = PathTree_Node (Just a) mempty
    go (x : xs) = PathTree_Node Nothing $ Map.singleton x $ go xs

-- | Merge two 'PathTree's preferring leaves on the right when they match the left.
mergePathTreesRightBias :: PathTree a -> PathTree a -> PathTree a
mergePathTreesRightBias (PathTree_Node ax x) (PathTree_Node ay y) =
  PathTree_Node (ay <|> ax) $ Map.unionWith mergePathTreesRightBias x y


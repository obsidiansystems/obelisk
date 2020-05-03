{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-} -- For recursive definitions of 'PathConcatResult'
module Obelisk.Command.Path.Internal where

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
-- A key property of 'Canonical' 'Path's is that when two are equal they represent
-- the same location on the file system.
data Canonical

-- | A 'Path' tag indicating that the path is absolute
data Absolute

-- | A 'Path' tag indicating that the path is relative
data Relative

class IsPath path where
  toFilePath :: path -> FilePath

  -- Lift *any* 'FilePath' into the path type.
  unsafeFromFilePath :: FilePath -> path

instance IsPath FilePath where
  toFilePath = id
  unsafeFromFilePath = id

instance IsPath (Path t) where
  toFilePath = coerce
  unsafeFromFilePath = coerce

unsafeConcatPath :: (IsPath path1, IsPath path2, Coercible FilePath path3) => path1 -> path2 -> path3
unsafeConcatPath a b = coerce $ toFilePath a FilePath.</> toFilePath b

class PathConcat path1 path2 where
  type PathConcatResult path1 path2
  infixr 5 </>
  (</>) :: path1 -> path2 -> PathConcatResult path1 path2

  default (</>) :: (IsPath path1, IsPath path2, Coercible FilePath (PathConcatResult path1 path2)) => path1 -> path2 -> PathConcatResult path1 path2
  (</>) = unsafeConcatPath


-- How these instances work:
--   If we know *anything* about the path, we will produce a 'Path' with some known tag.
--   If we don't know anything about the path, we use 'FilePath' directly.

-- 'Absolute' paths on the right *always* win.
instance IsPath path => PathConcat path (Path Canonical) where type PathConcatResult path (Path Canonical) = Path Canonical
instance IsPath path => PathConcat path (Path Absolute) where type PathConcatResult path (Path Absolute) = Path Absolute

instance PathConcat (Path Canonical) (Path Relative) where type PathConcatResult (Path Canonical) (Path Relative) = Path Absolute -- The 'Relative' path may be a symlink making the path no longer 'Canonical'
instance PathConcat (Path Canonical) FilePath where type PathConcatResult (Path Canonical) FilePath = Path Absolute -- Unknown could be 'Canonical', but that's just a form of being 'Absolute'

instance PathConcat (Path Absolute) (Path Relative) where type PathConcatResult (Path Absolute) (Path Relative) = Path Absolute
instance PathConcat (Path Absolute) FilePath where type PathConcatResult (Path Absolute) FilePath = Path Absolute -- Unknown could be 'Canonical', but that's just a form of being 'Absolute'

instance PathConcat (Path Relative) (Path Relative) where type PathConcatResult (Path Relative) (Path Relative) = Path Relative
instance PathConcat (Path Relative) FilePath where type PathConcatResult (Path Relative) FilePath = FilePath

instance PathConcat FilePath (Path Relative) where type PathConcatResult FilePath (Path Relative) = FilePath
instance PathConcat FilePath FilePath where type PathConcatResult FilePath FilePath = FilePath

rel :: FilePath -> Path Relative
rel = coerce

canonicalizePath :: (MonadIO m, IsPath path) => path -> m (Path Canonical)
canonicalizePath p = liftIO $ Path <$> Directory.canonicalizePath (toFilePath p)

makeAbsolute :: (MonadIO m, IsPath path) => path -> m (Path Absolute)
makeAbsolute p = liftIO $ Path <$> Directory.makeAbsolute (toFilePath p)

listDirectory :: (MonadIO m, IsPath path) => path -> m [Path Relative]
listDirectory p = liftIO $ coerce <$> Directory.listDirectory (toFilePath p)

splitDirectories :: IsPath path => path -> [String]
splitDirectories p = FilePath.splitDirectories (toFilePath p)

splitPath :: IsPath path => path -> [String]
splitPath p = FilePath.splitPath (toFilePath p)

createDirectoryIfMissing :: (MonadIO m, IsPath path) => Bool -> path -> m ()
createDirectoryIfMissing makeParents p = liftIO $ Directory.createDirectoryIfMissing makeParents (toFilePath p)

doesDirectoryExist :: (MonadIO m, IsPath path) => path -> m Bool
doesDirectoryExist p = liftIO $ Directory.doesDirectoryExist (toFilePath p)

doesFileExist :: (MonadIO m, IsPath path) => path -> m Bool
doesFileExist p = liftIO $ Directory.doesFileExist (toFilePath p)

takeDirectory :: (IsPath path) => path -> path
takeDirectory p = unsafeFromFilePath $ FilePath.takeDirectory (toFilePath p)

takeFileName :: IsPath path => path -> Path Relative
takeFileName p = Path $ FilePath.takeFileName (toFilePath p)

takeBaseName :: IsPath path => path -> String
takeBaseName p = FilePath.takeBaseName (toFilePath p)

takeExtension :: IsPath path => path -> String
takeExtension p = FilePath.takeExtension (toFilePath p)

makeRelative :: (IsPath path1, IsPath path2) => path1 -> path2 -> FilePath
makeRelative a b = FilePath.makeRelative (toFilePath a) (toFilePath b)

data FindMatch a = FindMatch_SkipTree | FindMatch_SearchTree | FindMatch_Match a

-- | Like @find@ utility but in Haskell. Note that it follows symlinks!
findFiles'
  :: forall a path m. (MonadIO m, Monoid a, IsPath path)
  => (FilePath -> Path Canonical -> m (FindMatch a)) -- ^ Decide how to handle an encountered path.
  -> path -- ^ Root path to start searching
  -> m a -- ^ Matches
findFiles' match root = go mempty (toFilePath root)
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
relativeTo :: Path Canonical -> Path Canonical -> Path Relative -- TODO: CPP this on Windows to make it Unknown
relativeTo dir base
  = rel
  $ bifoldr1 (</>)
  $ bimap (collapse . (".." <$) . catMaybes) (collapse . catMaybes)
  $ unzip
  $ dropWhile (uncurry (==))
  $ zipDefaultWith Nothing Nothing (,)
    (map Just $ splitDirectories base)
    (map Just $ splitDirectories dir)
  where collapse = foldr (FilePath.</>) ""


-- | Analogous to 'Map' 'FilePath' @a@ but unifies prefixes with an inductive tree structure.
data PathTree a = PathTree_Node
  (Maybe a) -- An optional leaf at this point in the tree
  (Map FilePath (PathTree a)) -- Branches to deeper leaves
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Semigroup a => Semigroup (PathTree a) where
  (PathTree_Node ax x) <> (PathTree_Node ay y) = PathTree_Node (ax <> ay) $ Map.unionWith (<>) x y

instance Semigroup a => Monoid (PathTree a) where
  mempty = PathTree_Node mempty mempty
  mappend = (<>)

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

-- | Convert a 'FilePath' into a 'PathTree'.
pathToTree :: IsPath path => a -> path -> PathTree a
pathToTree a p = go $ splitDirectories p
  where
    go [] = PathTree_Node (Just a) mempty
    go (x : xs) = PathTree_Node Nothing $ Map.singleton x $ go xs

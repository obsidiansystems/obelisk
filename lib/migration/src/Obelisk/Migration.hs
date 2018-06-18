{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Migration where

import Control.Monad (forM)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath

import Algebra.Graph.AdjacencyMap

type Hash = Text

-- Migration graph with edge metadata
data Migration action = Migration
  { _migration_graph :: AdjacencyMap Hash
  , _migration_actions :: Map (Hash, Hash) action -- Edge metadata
  }
  deriving (Eq, Show)

-- A path is a list of edges
type Path = [(Hash, Hash)]


data CyclicGraphError = CyclicGraphError
  deriving Show

data MultipleTerminalVertices = MultipleTerminalVertices
  deriving Show

data NonEquivalentPaths = NonEquivalentPaths Hash Hash
  deriving Show

data MissingEdgeInActionMap = MissingEdgeInActionMap (Hash, Hash)
  deriving Show

instance Exception CyclicGraphError
instance Exception MultipleTerminalVertices
instance Exception NonEquivalentPaths
instance Exception MissingEdgeInActionMap

-- | Get the directed-acylic graph of migration
getDag :: (MonadThrow m, Ord a) => AdjacencyMap a -> m (AdjacencyMap a)
getDag g = case topSort g of
  Just _ -> pure g
  Nothing -> throwM CyclicGraphError

-- | Return the list of actions that migrate from one hash to another
--
-- Return Nothing if no path can be found.
runMigration
  :: (MonadThrow m, Eq action, Ord action, Monoid action)
  => Migration action
  -> Hash
  -> Hash
  -> m (Maybe [(Hash, action)])
runMigration m a b = findShortestEquivalentPath m a b >>= traverse (traverse getActionWithHash)
  where
    getActionWithHash e@(_, v2) = getAction m e >>= pure . (v2,)

-- | Read the graph from a directory of files.
readGraph :: (String -> action) -> FilePath -> Text -> IO (Maybe (Migration action))
readGraph parseAction root name = doesDirectoryExist root >>= \case
  -- TODO: maybe get rid of parseAction (using typeclass?)
  False -> return Nothing
  True -> do
    allVertexPairs <- listDirectory root >>= return . catMaybes . fmap getEdgeVertices
    edgesInfo <- fmap catMaybes $ forM allVertexPairs $ \vs -> do
      getEdgeFor name vs >>= return . fmap (vs, )
    return $ pure $ Migration (edges $ fmap fst edgesInfo) (Map.fromList edgesInfo)
  where
    getEdgeVertices p = case T.splitOn "-" $ T.pack p of
      [v1, v2] -> case (T.length v1, T.length v2) of
        (40, 40) -> Just (v1, v2)
        _ -> Nothing
      _ -> Nothing
    getEdgeFor graph (v1, v2) = do
      let f = root </> (T.unpack $ v1 <> "-" <> v2) </> T.unpack graph
      doesFileExist f >>= \case
        True -> do
          c <- readFile f >>= pure . parseAction
          return $ Just c
        False -> return Nothing

-- | Find the shortest equivalent path between vertices
--
-- This path is equivalent to every other path per monoid concat of edge actions.
--
-- Return `Nothing` if no path exists between the vertices, and `Left` if paths
-- are not equivalent. The successful case will be a `Right`
findShortestEquivalentPath
  :: (MonadThrow m, Eq action, Ord action, Monoid action)
  => Migration action -> Hash -> Hash -> m (Maybe Path)
findShortestEquivalentPath m a b = findAllPaths m a b >>= \case
  [] -> pure Nothing
  paths -> verify paths >>= \case
    True -> pure $ Just $ minimumBy (\x y -> compare (length x) (length y)) paths
    False -> throwM $ NonEquivalentPaths a b
  where
    verify xs = ((== 1) . length) . uniqs <$> traverse (fmap mconcat . traverse (getAction m)) xs
    uniqs = Set.toList . Set.fromList

-- | Find all possible paths from a to b
--
-- Vertices a and b must already exist (as verified by hasVertex).
-- Return empty list if no paths exist.
findAllPaths :: MonadThrow m => Migration action -> Hash -> Hash -> m [Path]
findAllPaths m a b = pure . fmap listInPairs . go a b . adjacencyMap =<< getDag (_migration_graph m)
  where
    -- Do a slow and dummy search. TODO: improve algorithm performance
    go x y g = case x == y of
      True ->
        [[y]]
      False -> case Map.lookup x g of
        Just adjs ->
          fmap (x :) $ mconcat $ fmap (\z -> go z y g) $ Set.toList adjs
        Nothing -> []
    listInPairs = \case
      [] -> []
      [_] -> []  -- One vertex path has zero edges (thus, zero length path)
      [x, y] -> [(x,y)]
      x:y:xs -> (x,y) : listInPairs (y : xs)

hasVertex :: Hash -> Migration action -> Bool
hasVertex h (Migration g _) = Set.member h $ vertexSet g

-- | Get the action for the given edge (which must exist)
getAction :: MonadThrow m => Migration action -> (Hash, Hash) -> m action
getAction (Migration _ h) e = case Map.lookup e h of
  Just v -> pure v
  Nothing -> throwM $ MissingEdgeInActionMap e

-- | Get the last vertex of the given DAG. Nothing if empty graph.
--
-- Assumes that the graph is fully connected.
getLast :: (MonadThrow m, Ord a, Eq a) => AdjacencyMap a -> m (Maybe a)
getLast g = lastVertices >>= \case
  [] -> pure $ Nothing
  [x] -> pure $ Just x
  _ -> throwM MultipleTerminalVertices
  where
    lastVertices
      = pure
      . fmap fst
      . filter (\(_, nexts) -> nexts == [])
      . adjacencyList
      =<< getDag g

-- | Get the first vertex of the given DAG. Nothing if empty graph.
getFirst :: (MonadThrow m, Ord a, Eq a) => AdjacencyMap a -> m (Maybe a)
getFirst = getLast . transpose

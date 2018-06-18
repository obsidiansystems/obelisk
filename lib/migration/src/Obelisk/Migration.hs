{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Migration where

import Control.Monad (forM)
import Data.List (minimumBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath

-- TODO: Add error handling in place of error/fail functions

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

-- | Get the directed-acylic graph of migration
getDag :: Ord a => AdjacencyMap a -> AdjacencyMap a
getDag g = case topSort g of
  Just _ -> g
  Nothing -> error "Cyclic graph detected"

-- | Return the list of actions that migrate from one hash to another
--
-- Return Nothing if no path can be found.
runMigration
  :: (Eq action, Ord action, Monoid action)
  => Migration action
  -> Hash
  -> Hash
  -> Maybe (Either Text [(Hash, action)])
runMigration m a b = fmap (fmap getActionWithHash) <$> findShortestEquivalentPath m a b
  where
    getActionWithHash e@(_, v2) = (v2, getAction m e)

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
  :: (Eq action, Ord action, Monoid action)
  => Migration action -> Hash -> Hash -> Maybe (Either Text Path)
findShortestEquivalentPath m a b = case findAllPaths m a b of
  [] -> Nothing
  paths -> Just $ case verify paths of
    True -> Right $ minimumBy (\x y -> compare (length x) (length y)) paths
    False -> Left $ "Not all paths between " <> a <> " and " <> b <> " are equivalent in actions"
  where
    verify xs = ((== 1) . length) $ uniqs $ fmap (mconcat . fmap (getAction m)) xs
    uniqs = Set.toList . Set.fromList

-- | Find all possible paths from a to b
--
-- Vertices a and b must already exist (as verified by hasVertex).
-- Return empty list if no paths exist.
findAllPaths :: Migration action -> Hash -> Hash -> [Path]
findAllPaths m a b = fmap listInPairs $ go a b $ adjacencyMap $ getDag $ _migration_graph m
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
getAction :: Migration action -> (Hash, Hash) -> action
getAction (Migration _ h) e = case Map.lookup e h of
  Just v -> v
  Nothing -> error $ "Edge " <> show e <> " not found"

-- | Get the last vertex of the given DAG. Nothing if empty graph.
--
-- Assumes that the graph is fully connected.
getLast :: (Ord a, Eq a) => AdjacencyMap a -> Maybe a
getLast g = case lastVertices of
  [] -> Nothing
  [x] -> Just x
  _ -> error "Invalid graph: multiple terminal vertices"
  where
    lastVertices
      = fmap fst
      $ filter (\(_, nexts) -> nexts == [])
      $ adjacencyList
      $ getDag g

-- | Get the first vertex of the given DAG. Nothing if empty graph.
getFirst :: (Ord a, Eq a) => AdjacencyMap a -> Maybe a
getFirst = getLast . transpose

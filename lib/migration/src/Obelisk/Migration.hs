{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Migration where

import Control.Monad (forM)
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

-- | Get the directed-acylic graph of migration
getDag :: Ord a => AdjacencyMap a -> AdjacencyMap a
getDag g = case topSort g of
  Just _ -> g
  Nothing -> error "Cyclic graph detected"

-- Return actions that migrate from one hash to another
runMigration :: Migration action -> Hash -> Hash -> Maybe [(Hash, action)]
runMigration m a b = fmap getAction <$> findPath a b m
  where
    getAction (v1, v2) = case Map.lookup (v1, v2) $ _migration_actions m of
      Just action -> (v2, action)
      Nothing -> error $ "Missing edge in actions map: " <> show (v1, v2)

-- | Read the graph from a directory of files.
readGraph :: (String -> action) -> FilePath -> Text -> IO (Maybe (Migration action))
readGraph parseAction root name = doesDirectoryExist root >>= \case
  -- TODO: maybe get rid of parseAction
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

-- | Finds a path from a to b
--
-- Vertices a and b must already exist (as verified by hasVertex).
findPath :: Hash -> Hash -> Migration action -> Maybe [(Hash, Hash)]
findPath a b (Migration g' _) = fmap listInPairs $ go a b $ adjacencyMap $ getDag g'
  where
    -- Do a slow and dummy search. TODO: improve algorithm performance
    go x y g = case x == y of
      True -> Just []
      False -> case Map.lookup x g of
        Just adjs ->
          let
            subPaths = listToMaybe $ catMaybes $ fmap (\z -> go z y g) $ Set.toList adjs
          in
            fmap (x :) $ if subPaths == Just [] then Just [y] else subPaths
        Nothing -> Nothing
    listInPairs = \case
      [] -> []
      [_] -> error "Not possible to have one vertex in path"
      [x, y] -> [(x,y)]
      x:y:xs -> (x,y) : listInPairs (y : xs)

hasVertex :: Hash -> Migration action -> Bool
hasVertex h (Migration g _) = Set.member h $ vertexSet g

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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Obelisk.Migration where

import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Strict (evalStateT, get, put)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Semigroup ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
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

data GraphInternalError = GraphInternalError Text
  deriving Show

data CyclicGraphError = CyclicGraphError
  deriving Show

data MultipleTerminalVertices = MultipleTerminalVertices
  deriving Show

data NoTerminalVertex = NoTerminalVertex
  deriving Show

data NonEquivalentPaths = NonEquivalentPaths Hash Hash
  deriving Show

data MissingEdgeInActionMap = MissingEdgeInActionMap (Hash, Hash)
  deriving Show

data NonLinearGraph
  = NonLinearGraph_MultipleAdjacents Hash
  | NonLinearGraph_NotConnected (Set Hash)
  deriving Show

instance Exception GraphInternalError
instance Exception CyclicGraphError
instance Exception MultipleTerminalVertices
instance Exception NoTerminalVertex
instance Exception NonEquivalentPaths
instance Exception MissingEdgeInActionMap
instance Exception NonLinearGraph

class Action action where
  parseEdgeMeta :: String -> action

instance Action Text where
  parseEdgeMeta = T.pack

-- | Get the directed-acylic graph of migration
-- TODO: Consider calling getDag only when reading the graph for efficiency.
getDag :: (MonadThrow m, Ord a) => AdjacencyMap a -> m (AdjacencyMap a)
getDag g = case topSort g of
  Just _ -> pure g
  Nothing -> throwM CyclicGraphError

-- | Read the graph from a directory of files.
readGraph :: Action action => FilePath -> Text -> IO (Maybe (Migration action))
readGraph root name = doesDirectoryExist root >>= \case
  False -> return Nothing
  True -> do
    allVertexPairs <- listDirectory root >>= return . catMaybes . fmap getEdgeVertices
    edgesInfo <- fmap catMaybes $ forM allVertexPairs $ \vs -> do
      getEdgeFor name vs >>= return . fmap (vs, )
    return $ pure $ Migration (edges $ fmap fst edgesInfo) (Map.fromList edgesInfo)
  where
    getEdgeVertices p = case T.splitOn "-" $ T.pack p of
      [v1, v2] -> Just (v1, v2)
      _ -> Nothing
    getEdgeFor graph (v1, v2) = do
      let f = root </> (T.unpack $ v1 <> "-" <> v2) </> T.unpack graph
      doesFileExist f >>= \case
        True -> do
          c <- readFile f >>= pure . parseEdgeMeta
          return $ Just c
        False -> do
          return Nothing

-- | Ensure that the migration graph is correct
--
-- In a correct migration graph: for all pairs of nodes, all paths between
-- those nodes have the same migration value
--
-- Throw the following if integrity check fails
-- * `CyclicGraphError`
-- * `NonEquivalentPaths`
--
-- Complexity: O(V+E)
ensureGraphIntegrity
  :: (MonadThrow m, Monoid action, Ord action)
  => Migration action -> m ()
ensureGraphIntegrity m = do
  graph <- fmap adjacencyMap $ getDag $ _migration_graph m
  firstVertex <- getFirst $ _migration_graph m
  let
    visitFrom visited acc start = case Map.lookup start graph of
      Nothing -> pure ()
      Just adjs -> void $ flip traverse (Set.toList adjs) $ \adj -> do
        acc' <- fmap (mappend acc) $ getAction m (start, adj)
        visited' <- case Map.lookup adj visited of
          Nothing -> pure $ Map.insert adj acc' visited
          Just adjAcc -> if acc' == adjAcc
            then pure visited
            else throwM $ NonEquivalentPaths start adj
        visitFrom visited' acc' adj
  -- We only check all paths from the _first_ vertex; it is not necessary to
  -- check all paths between _all_ pairs of vertices as that can be proved by
  -- induction, as long as there is exactly one first vertex (which we check on
  -- further above).
  visitFrom mempty mempty firstVertex

-- | Ensure that the graph is a fully connected linear list
ensureGraphLinearity :: MonadThrow m => Migration action -> m ()
ensureGraphLinearity m = do
  graph <- fmap adjacencyMap $ getDag $ _migration_graph m
  firstVertex <- getFirst $ _migration_graph m
  let
    visitFrom start = case Map.lookup start graph of
      Nothing -> throwM $ GraphInternalError $ "Vertex not found: " <> start
      Just adjs -> case (Set.toList adjs) of
        [] -> [start]
        [adj] -> start : visitFrom adj
        _ -> throwM $ NonLinearGraph_MultipleAdjacents start
    visitedVertices = Set.fromList $ visitFrom firstVertex
    allVertices = vertexSet $ _migration_graph m
    unvisitedVertices = Set.difference allVertices visitedVertices
  if Set.null unvisitedVertices
    then pure ()
    else throwM $ NonLinearGraph_NotConnected unvisitedVertices

-- | Find the concatenated actions between two vertices
--
-- All paths between the vertices must have the same mappend'ed action;
-- otherwise this will throw.
--
-- Complexity: O(V+E)
findPathAction
  :: (MonadThrow m, Action action, Monoid action, Ord action)
  => Migration action -> Hash -> Hash -> m (Maybe action)
findPathAction m start end = flip evalStateT Map.empty $ do
  graph <- fmap adjacencyMap $ getDag $ _migration_graph m
  let
    findPathFromCached start' = do
      -- TODO: refactor the caching part into a separate `memoize` function
      cache <- get
      case Map.lookup start' cache of
        Just result ->
          pure result
        Nothing -> do
          result <- findPathFrom start'
          put $ Map.insert start' result cache
          pure result
    findPathFrom start' = if start' == end
      then
        pure $ Just mempty
      else case Map.lookup start' graph of
        Just adjs -> do
          actions <- fmap catMaybes $ flip traverse (Set.toList adjs) $ \adj -> do
            action0 <- getAction m (start', adj)
            actionM <- findPathFromCached adj
            pure $ fmap (mappend action0) $ actionM
          case uniqs actions of
            [] -> pure Nothing
            [v] -> pure $ Just v  -- Exactly one monoidal value; accept.
            _ -> throwM $ NonEquivalentPaths start' end
        Nothing ->
          pure Nothing
    uniqs = Set.toList . Set.fromList
  findPathFromCached start

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
getLast :: (MonadThrow m, Ord a, Eq a) => AdjacencyMap a -> m a
getLast g = lastVertices >>= \case
  [] -> throwM NoTerminalVertex
  [x] -> pure x
  _ -> throwM MultipleTerminalVertices
  where
    lastVertices
      = pure
      . fmap fst
      . filter (\(_, nexts) -> nexts == [])
      . adjacencyList
      =<< getDag g

-- | Get the first vertex of the given DAG. Nothing if empty graph.
getFirst :: (MonadThrow m, Ord a, Eq a) => AdjacencyMap a -> m a
getFirst = getLast . transpose

-- | Write an edge directly to the filesystem.
--
-- Actions are written with empty string.
--
-- Return the edge directory created.
writeEdge :: MonadIO m => FilePath -> [Text] -> Hash -> Hash -> m (Maybe FilePath)
writeEdge dir actionFiles v1 v2 = do
  unless (v1 /= v2) $
    fail $ "Cannot create self loop with: " <> T.unpack v1
  let edgeDir = dir </> (T.unpack $ v1 <> "-" <> v2)
  liftIO (doesDirectoryExist edgeDir) >>= \case
    True -> pure Nothing
    False -> do
      liftIO $ createDirectory edgeDir
      forM_ actionFiles $ \fp -> liftIO $
        writeFile (edgeDir </> T.unpack fp) ""
      pure $ Just edgeDir

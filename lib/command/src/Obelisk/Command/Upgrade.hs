{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command.Upgrade where

import Control.Monad (forM, unless, void)
import Control.Monad.Catch (onException, try)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Monoid (Any (..), getAny)
import Data.Semigroup ((<>))
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Posix.Process (executeFile)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Utils

import Obelisk.Command.Project (toImplDir)
import Obelisk.Command.Project (findProjectObeliskCommand)
import Obelisk.Command.Thunk (ThunkData (..), getThunkGitBranch, readThunk, updateThunk)

import Obelisk.Command.Upgrade.Hash
import Obelisk.Migration

newtype HandOffAction = HandoffAction Any
  deriving (Monoid, Ord, Eq, Show)

instance Action HandOffAction where
  parseEdgeMeta = HandoffAction . Any . (== "True")

data MigrationGraph
  = MigrationGraph_ObeliskUpgrade
  | MigrationGraph_ObeliskHandoff
  deriving (Eq, Show, Bounded, Enum)

graphName :: MigrationGraph -> Text
graphName = \case
  MigrationGraph_ObeliskHandoff -> "obelisk-handoff"
  MigrationGraph_ObeliskUpgrade -> "obelisk-upgrade"

fromGraphName :: Text -> MigrationGraph
fromGraphName = \case
  "obelisk-handoff" -> MigrationGraph_ObeliskHandoff
  "obelisk-upgrade" -> MigrationGraph_ObeliskUpgrade
  _ -> error "Invalid graph name specified"

graphNames :: [Text]
graphNames = fmap graphName $ [minBound .. maxBound]

ensureCleanProject :: MonadObelisk m => FilePath -> m ()
ensureCleanProject project =
  ensureCleanGitRepo project False "Cannot upgrade with uncommited changes"

-- | Decide whether we (ambient ob) should handoff to project obelisk before performing upgrade
decideHandOffToProjectOb :: MonadObelisk m => FilePath ->  m Bool
decideHandOffToProjectOb project = do
  ensureCleanProject project
  updateThunk (toImplDir project) $ \projectOb -> do
    (ambientGraph, ambientHash) <- getAmbientObInfo
    projectHash <- computeVertexHash projectOb
    case hasVertex projectHash ambientGraph of
      False -> do
        putLog Warning "Project ob not found in ambient ob's migration graph; handing off anyway"
        return True
      True -> findPathAction ambientGraph projectHash ambientHash >>= \case
        Nothing -> do
          putLog Warning "No migration path between project and ambient ob; handing off anyway"
          return True
        Just (HandoffAction dontHandoff) -> do
          return $ not $ getAny dontHandoff
  where
    getAmbientObInfo = do
      ambientOb <- getAmbientOb
      getMigrationGraph' ambientOb MigrationGraph_ObeliskHandoff >>= \case
        Nothing -> do
          failWith "Ambient ob has no migration (this can't be possible)"
        Just m -> do
          -- We don't have ambient's ob source code, so locate its hash from the
          -- graph. The last vertex should be it.
          ambientHash <- getLast (_migration_graph m)
          return (m, ambientHash)

-- | Return the path to the current ('ambient') obelisk process Nix directory
getAmbientOb :: MonadObelisk m => m FilePath
getAmbientOb = takeDirectory . takeDirectory <$> liftIO getObeliskExe

upgradeObelisk :: MonadObelisk m => FilePath -> Maybe Text -> m ()
upgradeObelisk project gitBranchM = do
  ensureCleanProject project
  gitBranch <- maybe (getObeliskBranch project) pure gitBranchM
  updateObelisk project gitBranch >>= handOffToNewOb project

getObeliskBranch :: MonadObelisk m => FilePath -> m Text
getObeliskBranch project = readThunk (toImplDir project) >>= \case
  Left e -> failWith $ T.pack $ show e
  Right (ThunkData_Checkout _) -> failWith "obelisk thunk must be packed"
  Right (ThunkData_Packed tptr) -> case getThunkGitBranch tptr of
    Just v -> pure v
    Nothing ->
      failWith "You must run `ob upgrade` with the Git branch specified explicitly, as the project's obelisk thunk does not specify any."

updateObelisk :: MonadObelisk m => FilePath -> Text -> m Hash
updateObelisk project gitBranch =
  withSpinner ("Fetching new obelisk [" <> gitBranch <> "]") $
    updateThunk (toImplDir project) $ \obImpl -> do
      fromHash <- computeVertexHash obImpl
      callProcessAndLogOutput (Debug, Debug) $
        gitProc obImpl ["checkout", T.unpack gitBranch]
      callProcessAndLogOutput (Debug, Debug) $
        gitProc obImpl ["pull"]
      return fromHash

handOffToNewOb :: MonadObelisk m => FilePath -> Hash -> m ()
handOffToNewOb project fromHash = do
  impl <- withSpinner' "Preparing for handoff" (Just $ ("Handed off to new obelisk " <>) . T.pack) $
    findProjectObeliskCommand project >>= \case
      Nothing -> failWith "Not an obelisk project"
      Just impl -> pure impl
  let opts = ["internal", "migrate", T.unpack fromHash]
  liftIO $ executeFile impl False ("--no-handoff" : opts) Nothing

migrateObelisk :: MonadObelisk m => FilePath -> Hash -> m ()
migrateObelisk project fromHash = void $ withSpinner' "Migrating to new obelisk" (Just id) $ do
  updateThunk (toImplDir project) $ \obImpl -> revertObImplOnFail obImpl $ do
    toHash <- computeVertexHash obImpl
    g <- getMigrationGraph' obImpl MigrationGraph_ObeliskUpgrade >>= \case
      Nothing -> failWith "New obelisk has no migration metadata"
      Just m -> pure m

    unless (hasVertex fromHash g) $ do
      failWith $ "Current obelisk hash " <> fromHash <> " missing in migration graph of new obelisk"
    unless (hasVertex toHash g) $ do
      -- This usually means that the target obelisk branch does not have
      -- migration vertex for its latest commit; typically due to developer
      -- negligence.
      failWith $ "New obelisk hash " <> toHash <> " missing in its migration graph"

    if fromHash == toHash
      then do
        pure $ "No upgrade available (new Obelisk is the same)"
      else do
        putLog Debug $ "Migrating from " <> fromHash <> " to " <> toHash
        findPathAction g fromHash toHash >>= \case
          Nothing -> do
            failWith "Unable to find migration path"
          Just action -> do
            unless (action == mempty) $ do
              putLog Notice "To upgrade your project to the new version of obelisk, please follow these instructions:\n"
              putLog Notice action
            pure $ "Migrated from " <> fromHash <> " to " <> toHash
  where
    revertObImplOnFail impl f = f `onException` do
      putLog Notice $ T.pack $ "Reverting changes to " <> impl
      callProcessAndLogOutput (Notice, Notice) $ gitProc project ["checkout", impl]

-- | Get the migration graph for project
getMigrationGraph
  :: (Action action, MonadObelisk m)
  => FilePath -> MigrationGraph -> m (Migration action)
getMigrationGraph obDir graph = getMigrationGraph' obDir graph >>= \case
  Nothing -> failWith "Migration graph missing"
  Just m -> pure m

getMigrationGraph'
  :: (Action action, MonadObelisk m)
  => FilePath -> MigrationGraph -> m (Maybe (Migration action))
getMigrationGraph' obDir graph = do
  let name = graphName graph
      dir = migrationDir obDir
  putLog Debug $ "Reading migration graph " <> name <> " from " <> T.pack dir
  liftIO $ readGraph dir name

computeVertexHash :: MonadObelisk m => FilePath -> m Hash
computeVertexHash = getDirectoryHash [migrationDirName]

-- | Verify the integrity of the migration graph in relation to the Git repo.
verifyMigration :: MonadObelisk m => FilePath -> m ()
verifyMigration obDir = do
  upgradeGraph <- withSpinner "Reading migration graph" $
    getMigrationGraph @Text obDir MigrationGraph_ObeliskUpgrade
  withSpinner "Checking graph integrity" $
    ensureGraphIntegrity upgradeGraph
  withSpinner "Checking graph is linear" $
    -- We don't support merge commits (i.e., diamonds in a graph) yet.
    -- Until we do, ensure that the graph is a linear list.
    try (ensureGraphLinearity upgradeGraph) >>= \case
      Left (NonLinearGraph_MultipleAdjacents vertex) ->
        failWith $ "Graph is not linear; branches at vertex: " <> tshow vertex
      Left (NonLinearGraph_NotConnected isolatedVertices) ->
        failWith $ "Graph is partly linear, but with isolated vertices: " <> tshow isolatedVertices
      Right () ->
        pure ()
  liftIO (doesDirectoryExist $ obDir </> ".git") >>= \case
    True ->
      withSpinner "Checking existence of HEAD vertex" $
        void $ getHeadVertex obDir
    False -> do
      hash <- computeVertexHash obDir
      unless (hasVertex hash upgradeGraph) $
        failWith $ "No vertex found for obelisk hash " <> hash

-- | Create an edge from HEAD vertex to the hash corresponding to the Git
-- working copy. The HEAD vertex must already exist.
createMigrationEdgeFromHEAD :: MonadObelisk m => FilePath -> m ()
createMigrationEdgeFromHEAD obDir = do
  headHash <- getHeadVertex obDir
  wcHash <- getDirectoryHash migrationIgnore obDir
  if (headHash == wcHash)
    then
      putLog Warning "No migration necessary (working copy has the same hash as that of HEAD)"
    else do
      writeEdge (migrationDir obDir) graphNames headHash wcHash >>= \case
        Nothing ->
          putLog Warning "No migration was created"
        Just edgeDir ->
          putLog Notice $ "Created edge: " <> T.pack edgeDir

-- | Return the hash corresponding to HEAD
--
-- Fail if the hash does not exist in project's migration graph.
getHeadVertex :: MonadObelisk m => FilePath -> m Hash
getHeadVertex obDir = do
  projectGraph <- getMigrationGraph @Text obDir MigrationGraph_ObeliskUpgrade
  [headHash] <- getHashAtGitRevision ["HEAD"] migrationIgnore obDir
  unless (hasVertex headHash projectGraph) $ do
    -- This means that the HEAD commit has no vertex in the graph,
    -- possibly due to developer negligence when commiting it.
    -- Perhaps we should use a post-commit hook or some such thing
    -- to reject such commits in the first place? For now, just
    -- error out.
    failWith $ "No vertex found for HEAD (" <> headHash <> ")"
  return headHash

-- | Create, or update, the migration graph with new edges and new vertices
-- corresponding to every commit in the Git history from HEAD.
--
-- NOTE: This creates a linear graph, and doesn't follow the Git graph
-- structure.
backfillGraph :: MonadObelisk m => Maybe Int -> FilePath -> m ()
backfillGraph lastN obDir = do
  revs <- fmap (takeM lastN . fmap (fst . T.breakOn " ") . T.lines) $
    readProc $ gitProc obDir ["log", "--pretty=oneline"]
  -- Note: we need to take unique hashes only; this is fine for backfilling.
  -- But future migrations should ensure that there are no duplicate hashes
  -- (which would cause cycles) such as those introduced by revert commits.
  vertices :: [Hash] <- withSpinnerNoTrail "Computing hash for git history" $
    fmap (unique . reverse) $ getHashAtGitRevision revs [migrationDirName] obDir
  void $ withSpinner'
    ("Backfilling with " <> tshow (length vertices) <> " vertices")
    (Just $ \n -> "Backfilled " <> tshow n <> " edges.") $ do
      let vertexPairs = zip vertices $ drop 1 vertices
      let edgesDir = migrationDir obDir
      liftIO $ createDirectoryIfMissing False edgesDir
      fmap (length . catMaybes) $ forM vertexPairs $ \(v1, v2) -> do
        writeEdge edgesDir graphNames v1 v2
  where
    -- Return unique items in the list /while/ preserving order
    unique = loop mempty
      where
        loop _ [] = []
        loop s (x : xs)
          | S.member x s = loop s xs
          | otherwise = x : loop (S.insert x s) xs
    takeM n' xs = case n' of
      Just n -> take n xs
      Nothing -> xs

migrationDir :: FilePath -> FilePath
migrationDir project = project </> migrationDirName

migrationIgnore :: [FilePath]
migrationIgnore = [migrationDirName]

migrationDirName :: FilePath
migrationDirName = "migration"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Upgrade where

import Control.Monad (unless, void)
import Control.Monad.Catch (onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Monoid (Any (..), getAny)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (getExecutablePath)
import System.FilePath
import System.Posix.Process (executeFile)
import System.Process (proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Utils

import Obelisk.Command.Project (toImplDir)
import Obelisk.Command.Project (findProjectObeliskCommand)
import Obelisk.Command.Thunk (updateThunk)

import Obelisk.Migration

newtype HandOffAction = HandoffAction Any
  deriving (Monoid, Ord, Eq, Show)

instance Action HandOffAction where
  parseEdgeMeta = HandoffAction . Any . (== "True")

data MigrationGraph
  = MigrationGraph_ObeliskUpgrade
  | MigrationGraph_ObeliskHandoff
  deriving (Eq, Show)

graphName :: MigrationGraph -> Text
graphName = \case
  MigrationGraph_ObeliskHandoff -> "obelisk-handoff"
  MigrationGraph_ObeliskUpgrade -> "obelisk-upgrade"

fromGraphName :: Text -> MigrationGraph
fromGraphName = \case
  "obelisk-handoff" -> MigrationGraph_ObeliskHandoff
  "obelisk-upgrade" -> MigrationGraph_ObeliskUpgrade
  _ -> error "Invalid graph name specified"

ensureCleanProject :: MonadObelisk m => FilePath -> m ()
ensureCleanProject project = ensureCleanGitRepo project False "Cannot upgrade with uncommited changes"

-- | Decide whether we (ambient ob) should handoff to project obelisk before performing upgrade
decideHandOffToProjectOb :: MonadObelisk m => FilePath ->  m Bool
decideHandOffToProjectOb project = do
  ensureCleanProject project
  updateThunk (toImplDir project) $ \projectOb -> do
    ambientOb <- getAmbientOb
    (ambientGraph, ambientHash) <- getAmbientObInfo
    projectHash <- computeVertexHash ambientOb MigrationGraph_ObeliskHandoff projectOb
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
          -- fmap (not . or . fmap parseHandoffMigration) $ traverse (getAction ambientGraph) path
  where
    getAmbientObInfo = do
      ambientOb <- getAmbientOb
      getMigrationGraph' ambientOb MigrationGraph_ObeliskHandoff >>= \case
        Nothing -> do
          failWith "Ambient ob has no migration (this can't be possible)"
        Just (m, _, ambientHash) -> do
          unless (hasVertex ambientHash m) $
            failWith "Ambient ob's hash is not in its own graph"
          return (m, ambientHash)

-- | Return the path to the current ('ambient') obelisk process Nix directory
getAmbientOb :: MonadObelisk m => m FilePath
getAmbientOb = takeDirectory . takeDirectory <$> liftIO getExecutablePath

upgradeObelisk :: MonadObelisk m => FilePath -> Text -> m ()
upgradeObelisk project gitBranch = do
  ensureCleanProject project
  updateObelisk project gitBranch >>= handOffToNewOb project

updateObelisk :: MonadObelisk m => FilePath -> Text -> m Hash
updateObelisk project gitBranch =
  withSpinner' "Updating Obelisk thunk" (Just $ const $ "Updated Obelisk thunk to latest of " <> gitBranch) $
    updateThunk (toImplDir project) $ \obImpl -> do
      ob <- getAmbientOb
      fromHash <- computeVertexHash ob MigrationGraph_ObeliskUpgrade obImpl
      callProcessAndLogOutput (Debug, Debug) $
        git1 obImpl ["checkout", T.unpack gitBranch]
      callProcessAndLogOutput (Debug, Debug) $
        git1 obImpl ["pull"]
      return fromHash

handOffToNewOb :: MonadObelisk m => FilePath -> Hash -> m ()
handOffToNewOb project fromHash = do
  impl <- withSpinner' "Preparing for handoff" (Just $ ("Handing off to new obelisk " <>) . T.pack) $
    findProjectObeliskCommand project >>= \case
      Nothing -> failWith "Not an Obelisk project"
      Just impl -> pure impl
  let opts = ["internal", "migrate", T.unpack fromHash]
  liftIO $ executeFile impl False ("--no-handoff" : opts) Nothing

migrateObelisk :: MonadObelisk m => FilePath -> Hash -> m ()
migrateObelisk project fromHash = void $ withSpinner' "Migrating to new Obelisk" (Just id) $ do
  updateThunk (toImplDir project) $ \obImpl -> revertObImplOnFail obImpl $ do
    toHash <- computeVertexHash obImpl MigrationGraph_ObeliskUpgrade obImpl
    (g, _, _) <- getMigrationGraph' obImpl MigrationGraph_ObeliskUpgrade >>= \case
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
            putLog Notice $ "Actionable migrations (if any) are shown below:\n"
            putLog Notice $ action
            putLog Notice $ "\nPlease commit the changes to this repo, and manually perform the above migrations if any to migrate your project to the upgraded Obelisk.\n"
            pure $ "Migrated from " <> fromHash <> " to " <> toHash
  where
    revertObImplOnFail impl f = f `onException` do
      putLog Notice $ T.pack $ "Reverting changes to " <> impl
      callProcessAndLogOutput (Notice, Notice) $ git1 project ["checkout", impl]

-- | Get the migration graph for project, along with the first and last hash.
getMigrationGraph'
  :: (Action action, MonadObelisk m)
  => FilePath -> MigrationGraph -> m (Maybe (Migration action, Hash, Hash))
getMigrationGraph' project graph = runMaybeT $ do
  let name = graphName graph
  putLog Debug $ "Reading migration graph " <> name <> " from " <> T.pack project
  g <- MaybeT $ liftIO $ readGraph (migrationDir project) name
  first <- MaybeT $ getFirst $ _migration_graph g
  last' <- MaybeT $ getLast $ _migration_graph g
  pure $ (g, first, last')


computeVertexHash :: MonadObelisk m => FilePath -> MigrationGraph -> FilePath -> m Hash
computeVertexHash obDir graph repoDir = fmap T.pack $ readProcessAndLogStderr Error $
  proc "sh" [hashScript, repoDir]
  where
    hashScript = (migrationDir obDir) </> (T.unpack (graphName graph) <> ".hash.sh")

migrationDir :: FilePath -> FilePath
migrationDir project = project </> "migration"

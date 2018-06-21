{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Upgrade where

import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Catch (onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Monoid (Any (..), getAny)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.Environment (getExecutablePath)
import System.FilePath
import System.IO.Temp
import System.Posix.Process (executeFile)
import System.Process (cwd, proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp
import Obelisk.Command.Utils

import Obelisk.Command.Project (toImplDir)
import Obelisk.Command.Project (findProjectObeliskCommand)
import Obelisk.Command.Thunk (ThunkData (..), getThunkGitBranch, readThunk, updateThunk)

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
ensureCleanProject project =
  ensureCleanGitRepo project False "Cannot upgrade with uncommited changes"

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
      failWith "You must specify a git branch to `ob upgrade` as obelisk thunk does not specify any."

updateObelisk :: MonadObelisk m => FilePath -> Text -> m Hash
updateObelisk project gitBranch =
  withSpinner ("Fetching new Obelisk [" <> gitBranch <> "]") $
    updateThunk (toImplDir project) $ \obImpl -> do
      ob <- getAmbientOb
      fromHash <- computeVertexHash ob MigrationGraph_ObeliskUpgrade obImpl
      callProcessAndLogOutput (Debug, Debug) $
        gitProc obImpl ["checkout", T.unpack gitBranch]
      callProcessAndLogOutput (Debug, Debug) $
        gitProc obImpl ["pull"]
      return fromHash

handOffToNewOb :: MonadObelisk m => FilePath -> Hash -> m ()
handOffToNewOb project fromHash = do
  impl <- withSpinner' "Preparing for handoff" (Just $ ("Handed off to new Obelisk " <>) . T.pack) $
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
      callProcessAndLogOutput (Notice, Notice) $ gitProc project ["checkout", impl]

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

-- TODO: Move this to migration library? (we rely on wrapProgram exes)

-- | Get the unique hash of the given directory
--
-- Excludes the specified top-level files/ directories. Uses the same algorithm as
-- Nix uses (`nix hash-path`).
--
-- This function will do a full copy of the directory to a temporary location before
-- computing the hash. Because it will be deleting the files in exclude list, and
-- other files if the directory is a git repo. This needs to be done as `nix hash-path`
-- doesn't support taking an excludes list.
getDirectoryHash :: MonadObelisk m => [FilePath] -> FilePath -> m Hash
getDirectoryHash excludes dir = withSystemTempDirectory "obelisk-hash-" $ \tmpDir -> do
  withSpinner (T.pack $ "Copying " <> dir <> " to " <> tmpDir) $ do
    runProc $ copyDir dir tmpDir
  getDirectoryHashDestructive excludes tmpDir

getDirectoryHashDestructive :: MonadObelisk m => [FilePath] -> FilePath -> m Hash
getDirectoryHashDestructive excludes dir = do
  liftIO (doesDirectoryExist $ dir </> ".git") >>= \case
    True -> do
      tidyUpGitWorkingCopy dir
      withSpinnerNoTrail "Removing .git directory" $
        liftIO $ removePathForcibly $ dir </> ".git"
    False -> pure ()
  -- Remove excluded paths
  withSpinnerNoTrail "Removing excluded paths" $ do
    forM_ (fmap (dir </>) excludes) $
      liftIO . removePathForcibly
  nixHash dir

getHashAtGitRevision :: MonadObelisk m => [Text] -> [FilePath] -> FilePath -> m [Hash]
getHashAtGitRevision revs excludes dir = withSystemTempDirectory "obelisk-hashrev-" $ \tmpDir -> do
  withSpinner (T.pack $ "Copying " <> dir <> " to " <> tmpDir) $ do
    runProc $ copyDir dir tmpDir
  tidyUpGitWorkingCopy tmpDir
  -- Discard changes to tracked files
  runProcSilently $ gitProc tmpDir ["reset", "--hard"]
  forM revs $ \rev -> do
    runProcSilently $ gitProc tmpDir ["checkout", T.unpack rev]
    -- Checking out an arbitrary revision can leave untracked files (from
    -- previous revison) around, so tidy them up.
    tidyUpGitWorkingCopy tmpDir
    withFilesStashed tmpDir (excludes <> [".git"]) $
      nixHash tmpDir
  where
    withFilesStashed base fs m = withSystemTempDirectory "obelisk-hashrev-stash-" $ \stashDir -> do
      forM_ fs $ \p ->
        liftIO $ renamePath (base </> p) (stashDir </> p)
      result <- m
      forM_ fs $ \p ->
        liftIO $ renamePath (stashDir </> p) (base </> p)
      return result

nixHash :: MonadObelisk m => FilePath -> m Hash
nixHash dir = withSpinnerNoTrail "Running `nix hash-path`" $
  fmap T.pack $ readProcessAndLogStderr Error $
    proc "nix" ["hash-path", "--type", "md5", dir]

-- | Clean up the following files in the git working copy
--
-- * Paths ignored by .gitignored, but still present in the filesystem
-- * Untracked files (not added to git index)
-- * Any empty directories (these are not tracked by git)
--
-- Note that this leaves modified (staged or unstaged) files as they are.
tidyUpGitWorkingCopy :: MonadObelisk m => FilePath -> m ()
tidyUpGitWorkingCopy dir = withSpinnerNoTrail "Tidying up git working copy" $ do
  ignored <- gitLsFiles dir ["--ignored", "--exclude-standard", "--others"]
  untracked <- gitLsFiles dir ["--exclude-standard", "--others"]
  putLog Debug $ T.pack $ "Found " <> show (length ignored) <> " ignored files."
  putLog Debug $ T.pack $ "Untracked:\n" <> unlines untracked
  putLog Debug $ T.pack $ "Ignored:\n" <> unlines ignored
  withSpinnerNoTrail "Removing untracked and ignored files" $ do
    forM_ (fmap (dir </>) $ ignored <> untracked) $
      liftIO . removePathForcibly
  -- Empty directories won't be included in these lists. Git doesn't track them
  -- So we must delete these separately.
  runProc $ proc "find" [dir, "-depth", "-empty", "-type", "d", "-delete"]
  where
    gitLsFiles pwd opts = fmap lines $ readProcessAndLogStderr Error $
      (proc "git" $ ["ls-files", "."] <> opts) { cwd = Just pwd }


computeVertexHash :: MonadObelisk m => FilePath -> MigrationGraph -> FilePath -> m Hash
computeVertexHash obDir graph repoDir = fmap T.pack $ readProcessAndLogStderr Error $
  proc "sh" [hashScript, repoDir]
  where
    hashScript = (migrationDir obDir) </> (T.unpack (graphName graph) <> ".hash.sh")


migrationDir :: FilePath -> FilePath
migrationDir project = project </> "migration"

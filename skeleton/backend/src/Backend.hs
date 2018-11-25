{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Backend where

import Common.Api
import Backend.Schema
import Common.Route
import Data.Text (Text)
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Pool
import Obelisk.Backend
import Obelisk.Route
import Obelisk.Api
import Obelisk.Db
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
import qualified Data.Map as Map
import Data.Sequence (Seq)

import Database.Beam
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.Types
import Control.Monad.Reader

import Data.Proxy

import Data.Functor.Identity
import Data.Functor.Const
import Control.Lens

import Debug.Trace

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDbUri "db" $ \dbUri -> do
        withConnectionPool dbUri $ \pool -> do
          withResource pool $ \conn -> do
            runBeamPostgres conn $ autoMigrate migrationBackend checkedDb
        let onNotify :: forall a. VS Int Text a -> Transaction -> ReadDb (V Int Text a)
            onNotify (VS _) txn = do
              traceM $ show txn
              traceM $ show $ decodeTransaction txn
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              traceM $ show tasks
              pure $ V $ MonoidalMap mempty
            onSubscribe :: forall a. VS Int Text a -> ReadDb (V Int Text a)
            onSubscribe (VS _) = do
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              traceM $ show tasks
              pure $ V $ MonoidalMap mempty
            onRequest :: forall a. MyRequest a -> WriteDb a
            onRequest = \case
              MyRequest_Echo a -> do
                runInsert' $ insert (_db_tasks db) $ insertExpressions $ (:[]) $ Task
                  { _task_id = default_
                  , _task_description = val_ a
                  , _task_completed = val_ False
                  }
                pure a
        withApiHandler dbUri onRequest onNotify onSubscribe $ \handler -> do
          serve $ \case
            BackendRoute_Api :/ () -> handler
            _ -> return ()
  , _backend_routeEncoder = backendRouteEncoder
  }

class MonadBeamRead m where
  runSelectReturningList' :: FromBackendRow Postgres a => SqlSelect Postgres a -> m [a]
  runSelectReturningOne' :: FromBackendRow Postgres a => SqlSelect Postgres a -> m (Maybe a)

unsafeRunPgReadDb :: Pg a -> ReadDb a
unsafeRunPgReadDb a = ReadDb $ do
  conn <- ask
  lift $ runBeamPostgres conn a

unsafeRunPgWriteDb :: Pg a -> WriteDb a
unsafeRunPgWriteDb a = WriteDb $ do
  conn <- ask
  lift $ runBeamPostgres conn a

instance MonadBeamRead WriteDb where
  runSelectReturningList' = unsafeRunPgWriteDb . runSelectReturningList
  runSelectReturningOne' = unsafeRunPgWriteDb . runSelectReturningOne

instance MonadBeamRead ReadDb where
  runSelectReturningList' = unsafeRunPgReadDb . runSelectReturningList
  runSelectReturningOne' = unsafeRunPgReadDb . runSelectReturningOne

class MonadBeamWrite m where
  runInsert' :: SqlInsert Postgres table -> m ()
  runUpdate' :: SqlUpdate Postgres table -> m ()
  runDelete' :: SqlDelete Postgres table -> m ()

instance MonadBeamWrite WriteDb where
  runInsert' = unsafeRunPgWriteDb . runInsert
  runUpdate' = unsafeRunPgWriteDb . runUpdate
  runDelete' = unsafeRunPgWriteDb . runDelete

decodeTransaction :: Transaction -> Db (Const (Seq Change))
decodeTransaction (_, tableChanges) = runIdentity $ zipTables (Proxy :: Proxy Postgres) f db db
  where
    f :: IsDatabaseEntity Postgres entityType => DatabaseEntity Postgres Db entityType -> DatabaseEntity Postgres Db entityType -> Identity (Const (Seq Change) entityType)
    f (DatabaseEntity descriptor) _ = pure $ Const $
      let n = view dbEntityName descriptor
          s = Just "public" -- view dbEntitySchema descriptor --TODO: Beam has 'Nothing' while the replication connection reports 'Just "public"'
      in trace (show (s, n)) $ Map.findWithDefault mempty (QualifiedIdentifier s n) tableChanges

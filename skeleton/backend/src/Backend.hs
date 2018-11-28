{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
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

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
module Backend where

import Control.Exception
import Control.Applicative
import Control.Concurrent
import Common.Api
import Common.Schema
import Backend.Schema
import Common.Route
import Data.Foldable
import qualified Data.Map.Monoidal as Map
import Data.Maybe
import Data.Pool
import Data.Semigroup
import Obelisk.Backend
import Obelisk.Route
import Obelisk.Api
import Obelisk.Db
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
import qualified Data.Map as OldMap
import Data.Text.Encoding
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Database.PostgreSQL.Simple.FromField (FromField (..), runConversion)
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.Internal
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import qualified Database.PostgreSQL.LibPQ as PQ
import Database.Beam
import Database.Beam.Backend.Types
import Database.Beam.Backend.SQL.Types
import Database.Beam.Postgres
import Database.Beam.Postgres.Full (PgInsertReturning, returning, runPgInsertReturningList)
import Database.Beam.Postgres.Migrate
import Database.Beam.Migrate.Simple
import Database.Beam.Schema.Tables
import Database.PostgreSQL.Simple.Types
import Control.Monad.Reader
import Reflex.Patch
import Reflex.Patch.MapWithMove2

import Data.Proxy

import Control.Lens
import qualified GHC.Generics as Generic
import GHC.Generics hiding (R, C)
import GHC.Types

import Debug.Trace

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      withDbUri "db" $ \dbUri -> do
        withConnectionPool dbUri $ \pool -> do
          withResource pool $ \conn -> do
            try (runBeamPostgres conn $ autoMigrate migrationBackend checkedDb) >>= \case
              Left (e :: SomeException) -> do
                print e
                forever $ threadDelay maxBound --TODO: Probably not necessary once we have `ob psql`
              Right _ -> pure ()
        let onNotify :: forall a. VS (Id Task) Task a -> Transaction -> ReadDb (V (Id Task) Task a)
            onNotify (VS requested) txn = do
              traceM "Hello"
              decoded <- ReadDb $ do
                conn <- ask
                lift $ decodeTransaction conn txn
              traceM $ show $ _db_tasks decoded
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              pure $ V $ Map.intersectionWith (,) requested $ Map.fromList $ fmap (\x -> (TaskId $ _task_id x, First $ Just x)) tasks
            onSubscribe :: forall a. VS (Id Task) Task a -> ReadDb (V (Id Task) Task a)
            onSubscribe (VS requested) = do
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              pure $ V $ Map.intersectionWith (,) requested $ Map.fromList $ fmap (\x -> (TaskId $ _task_id x, First $ Just x)) tasks
            onRequest :: forall a. MyRequest a -> WriteDb a
            onRequest = \case
              MyRequest_Add a -> do
                [taskId] <- runInsertReturningList' $ flip returning _task_id $ insert (_db_tasks db) $ insertExpressions $ (:[]) $ Task
                  { _task_id = default_
                  , _task_description = val_ a
                  , _task_completed = val_ False
                  }
                pure $ TaskId taskId
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
  runInsertReturningList' :: FromBackendRow Postgres a => PgInsertReturning a -> m [a]
  runUpdate' :: SqlUpdate Postgres table -> m ()
  runDelete' :: SqlDelete Postgres table -> m ()

instance MonadBeamWrite WriteDb where
  runInsert' = unsafeRunPgWriteDb . runInsert
  runInsertReturningList' = unsafeRunPgWriteDb . runPgInsertReturningList
  runUpdate' = unsafeRunPgWriteDb . runUpdate
  runDelete' = unsafeRunPgWriteDb . runDelete

class GPatchDatabase x where
  gDatabasePatchDecoder :: x

instance GPatchDatabase (x p) => GPatchDatabase (D1 f x p) where
  gDatabasePatchDecoder = M1 gDatabasePatchDecoder

instance GPatchDatabase (x p) => GPatchDatabase (C1 f x p) where
  gDatabasePatchDecoder = M1 gDatabasePatchDecoder

instance (GPatchDatabase (x p), GPatchDatabase (y p)) => GPatchDatabase ((x :*: y) p) where
  gDatabasePatchDecoder = gDatabasePatchDecoder :*: gDatabasePatchDecoder

instance PatchEntity be a => GPatchDatabase (S1 f (K1 Generic.R (EntityPatchDecoder be a)) p) where
  gDatabasePatchDecoder = M1 (K1 patchEntity)

class GPatchFields x where
  gPatchFields :: x

instance GPatchFields (x p) => GPatchFields (D1 f x p) where
  gPatchFields = M1 gPatchFields

instance GPatchFields (x p) => GPatchFields (C1 f x p) where
  gPatchFields = M1 gPatchFields

instance (GPatchFields (x p), GPatchFields (y p)) => GPatchFields ((x :*: y) p) where
  gPatchFields = gPatchFields :*: gPatchFields

instance BackendFromField be a => GPatchFields (S1 f (K1 Generic.R (FieldDecoder be a)) p) where
  gPatchFields = M1 (K1 FieldDecoder)

class PatchEntity be e where
  patchEntity :: EntityPatchDecoder be e

from' :: Generic x => x -> Rep x ()
from' = Generic.from

to' :: Generic x => Rep x () -> x
to' = Generic.to

instance ( Generic (tbl (FieldDecoder be))
         , GPatchFields (Rep (tbl (FieldDecoder be)) ())
         , Table tbl
         , Ord (PrimaryKey tbl Identity)
         , Eq (tbl Maybe)
         ) => PatchEntity be (TableEntity tbl) where
  patchEntity = EntityPatchDecoder_Table $ to' gPatchFields

defaultDatabasePatchDecoder
  :: forall be db
  .  ( Generic (db (EntityPatchDecoder be))
     , GPatchDatabase (Rep (db (EntityPatchDecoder be)) ())
     )
  => db (EntityPatchDecoder be)
defaultDatabasePatchDecoder = to' gDatabasePatchDecoder

class PatchableEntity be e where
  patchDecoder :: EntityPatchDecoder be e

data EntityPatchDecoder be e where
  EntityPatchDecoder_NotDecodable :: EntityPatchDecoder be e
  EntityPatchDecoder_Table :: (Table tbl, Ord (PrimaryKey tbl Identity), Eq (tbl Maybe)) => tbl (FieldDecoder be) -> EntityPatchDecoder be (TableEntity tbl)

data FieldDecoder be a where
  FieldDecoder :: BackendFromField be a => FieldDecoder be a

data EntityPatch e where
  EntityPatch_NotPatchable :: EntityPatch e
  EntityPatch_Table :: PatchMapWithMove2 (PrimaryKey tbl Identity) (PartialUpdate tbl) -> EntityPatch (TableEntity tbl)

deriving instance (Show (PrimaryKey tbl Identity), Show (tbl Maybe), Show (tbl Identity)) => Show (EntityPatch (TableEntity tbl))

deriving instance FromField a => FromField (SqlSerial a)

decodeRow
  :: forall tbl tbl'
  .  Beamable tbl
  => Connection
  -> tbl (TableField tbl')
  -> tbl (FieldDecoder Postgres)
  -> HashMap Identifier (TypeName, Maybe Literal)
  -> IO (tbl Maybe)
decodeRow conn settings decoders r = zipBeamFieldsM d settings decoders
  where d :: forall a. Columnar' (TableField tbl') a -> Columnar' (FieldDecoder Postgres) a -> IO (Columnar' Maybe a)
        d (Columnar' fieldSettings) (Columnar' FieldDecoder) = do
          let n = _fieldName fieldSettings
          result <- case HashMap.lookup (Identifier n) r of
            Just (TypeName typeName, v) -> do
              let f = Field
                    { typeOid = case typeName of
                        "text" -> TI.typoid TI.text
                        "int8" -> TI.typoid TI.int8
                        "bigint" -> TI.typoid TI.int8
                        "character varying" -> TI.typoid TI.text
                        "boolean" -> TI.typoid TI.bool
                        _ -> error $ "Unrecognized type: " <> show typeName --TODO: Retrieve types properly
                    , format = PQ.Text --TODO: Is this right?
                    , tableOid = Nothing
                    , name = Just $ encodeUtf8 n
                    }
              case v of
                Just (Literal_Present l) -> Just <$> runConversion (fromField f $ Just l) conn
                Nothing -> Just <$> runConversion (fromField f Nothing) conn
                Just Literal_UnchangedToastDatum -> pure Nothing
            Nothing -> pure Nothing
          case result of
            Just (Ok a) -> pure $ Columnar' $ Just a
            Just (Errors e) -> fail $ show e
            Nothing -> pure $ Columnar' Nothing

sequenceMaybeBeamable :: (Beamable table, Applicative f, f ~ Maybe) => table f -> f (table Identity)
sequenceMaybeBeamable t = zipBeamFieldsM (\(Columnar' a) _ -> Columnar' <$> a) t t

newtype PartialUpdate tbl = PartialUpdate (tbl Maybe)

instance Beamable tbl => Semigroup (PartialUpdate tbl) where
  PartialUpdate a <> PartialUpdate b = PartialUpdate $ runIdentity $ zipBeamFieldsM (\(Columnar' x) (Columnar' y) -> pure $ Columnar' $ x <|> y) a b

instance Beamable tbl => Monoid (PartialUpdate tbl) where
  mempty = PartialUpdate $ runIdentity $ zipBeamFieldsM (\_ _ -> pure $ Columnar' Nothing) tblSkeleton tblSkeleton
  mappend = (<>)

deriving instance Show (tbl Maybe) => Show (PartialUpdate tbl)
deriving instance Eq (tbl Maybe) => Eq (PartialUpdate tbl)

instance Beamable tbl => Patch (PartialUpdate tbl) where
  type PatchTarget (PartialUpdate tbl) = tbl Identity
  apply (PartialUpdate new) old = zipBeamFieldsM (\(Columnar' newField) (Columnar' oldField) -> pure $ Columnar' $ fromMaybe oldField newField) new old

--TODO: Inserts should always be complete; deletes should always have a key
decodeChange
  :: forall tbl
  .  Table tbl
  => Connection
  -> tbl (TableField tbl)
  -> tbl (FieldDecoder Postgres)
  -> Change
  -> IO (PatchMapWithMove2 (PrimaryKey tbl Identity) (PartialUpdate tbl))
decodeChange conn settings decoders =
  let decodeRecord = decodeRow conn settings decoders
      decodeKey k = do
        v <- decodeRow conn (primaryKey settings) (primaryKey decoders) (fmap (fmap Just) k)
        case sequenceMaybeBeamable v of
          Nothing -> fail "Got partial key"
          Just a -> pure a
  in \case
  Change_Insert rRaw -> do
    rm <- decodeRecord rRaw
    case sequenceMaybeBeamable rm of
      Nothing -> fail "Insert got partial row"
      Just r -> pure $ patchMapWithMove2InsertAll $ OldMap.singleton (primaryKey r) r
  Change_Update mkRaw rRaw -> do
    mk <- traverse decodeKey mkRaw
    vm <- decodeRecord rRaw
    case mk <|> sequenceMaybeBeamable (primaryKey vm) of
      Nothing -> fail "Update with no key"
      Just k -> pure $ PatchMapWithMove2 $ OldMap.singleton k $ NodeInfo
        { _nodeInfo_from = From_Move k $ PartialUpdate vm
        , _nodeInfo_to = Just k
        }
  Change_Delete kRaw -> do
    k <- decodeKey kRaw
    pure $ PatchMapWithMove2 $ OldMap.singleton k $ NodeInfo
      { _nodeInfo_from = From_Delete
      , _nodeInfo_to = Nothing
      }

decodeTransaction :: Connection -> Transaction -> IO (Db EntityPatch)
decodeTransaction conn (_, tableChanges) = zipTables (Proxy :: Proxy Postgres) f db (defaultDatabasePatchDecoder @Postgres @Db)
  where
    f :: forall entityType. IsDatabaseEntity Postgres entityType => DatabaseEntity Postgres Db entityType -> EntityPatchDecoder Postgres entityType -> IO (EntityPatch entityType)
    f (DatabaseEntity descriptor) decoder =
      let n = view dbEntityName descriptor
          s = Just "public" -- view dbEntitySchema descriptor --TODO: Beam has 'Nothing' while the replication connection reports 'Just "public"'
          vals = OldMap.findWithDefault mempty (QualifiedIdentifier s n) tableChanges
          x :: IO (EntityPatch entityType)
          x = case decoder of
                EntityPatchDecoder_NotDecodable -> pure EntityPatch_NotPatchable
                EntityPatchDecoder_Table decoders -> EntityPatch_Table . mconcat . toList <$> traverse (decodeChange conn (dbTableSettings descriptor) decoders) vals
      in x

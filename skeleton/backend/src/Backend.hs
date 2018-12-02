{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
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

import Prelude hiding (id, (.))

import Control.Category
import Control.Category.Monoidal (Monoidal)
import Control.Category.Braided
import Control.Categorical.Bifunctor
import Control.Category.Associative
import Control.Category.Cartesian
import Control.Exception
import Control.Applicative
import Control.Concurrent
import Common.Api
import Common.Schema
import Backend.Schema
import Common.Route
import Data.Foldable
import Data.Functor.Misc
import qualified Data.Map.Monoidal as Map
import Data.Maybe
import Data.Pool
import Data.Semigroup
import Data.Vessel
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

import Control.Lens hiding (Bifunctor, bimap)
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
        let onNotify :: VS (Id Task) (First Task) Proxy -> Transaction -> ReadDb (V (Id Task) (First Task) Identity)
            onNotify (MapV requested) txn = do
              decoded <- ReadDb $ do
                conn <- ask
                lift $ decodeTransaction conn txn
              traceM $ show $ _db_tasks decoded
              traceM $ show $ _db_users decoded
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              pure $ MapV $ Map.fromList $ fmap (\x -> (TaskId $ _task_id x, Identity $ First x)) tasks
            onSubscribe :: VS (Id Task) (First Task) Proxy -> ReadDb (V (Id Task) (First Task) Identity)
            onSubscribe (MapV requested) = do
              tasks <- runSelectReturningList' $ select $ all_ $ _db_tasks db
              pure $ MapV $ Map.fromList $ fmap (\x -> (TaskId $ _task_id x, Identity $ First x)) tasks
            onRequest :: forall a. MyRequest a -> WriteDb a
            onRequest = \case
              MyRequest_Add a -> do
                [taskId] <- runInsertReturningList' $ flip returning _task_id $ insert (_db_tasks db) $ insertExpressions $ (:[]) $ Task
                  { _task_id = default_
                  , _task_description = val_ a
                  , _task_completed = val_ False
                  , _task_assignee = val_ undefined
                  }
                pure $ TaskId taskId
        withApiHandler dbUri onRequest onNotify onSubscribe $ \handler -> do
          serve $ \case
            BackendRoute_Api :/ () -> handler
            _ -> return ()
  , _backend_routeEncoder = backendRouteEncoder
  }

class CoCartesian' (k :: o -> o -> *) where
  type Sum' k :: (o -> *) -> o
  inSum :: tag x -> x `k` Sum' k tag
  outSum :: (forall x. tag x -> x `k` a) -> Sum' k tag `k` a

class Cartesian' (k :: o -> o -> *) where
  type Product' k :: (o -> *) -> o
  outProd :: field x -> Product' k field `k` x
  inProd :: (forall x. field x -> a `k` x) -> a `k` Product' k field
  inProd' :: forall (field :: o -> *) (a :: o). (Product' (->) (WrapArg field (k a))) -> a `k` Product' k field

newtype Product'Hask field = Product'Hask (forall x. field x -> x)

instance Cartesian' (->) where
  type Product' (->) = Product'Hask

{-
type MyAppView = CrudEverything MyAppDb
-}

{-
crudEverything :: IV (PostgresDb a) (a CrudIF, someOtherIF)
crudEverything = (distributeIV $ MyDB
  { _myDb_thing = crudIV :: IV (TableEntity ThingT) (CrudIF Thing)
  }) &&& someOtherIV

watch $ (mempty { _myDb_thing = crudPage 10 0 }, mempty)
-}

{-
newtype PatchTarget' p = PatchTarget' (PatchTarget p)

data Server a b = forall state. Server
  { _server_subscribe :: b Proxy -> ReadDb (b (Product state PatchTarget')) -- Output 'b' must have same shape as input
  , _server_notify :: Transaction -> b state -> ReadDb (b (Product state Identity)) -- Output state must have same shape as input state; output `b Identity` must be no larger than input `b State`, and should have size approximately proportional to the Transaction size.  Output `b State` should generally not be fully traversed.
  , _server_request :: forall x. request x -> WriteDb x
  }
-}

{-
data Delta x

data IF query view request response

data DBIn query request = (Event (Delta query), Event request)

data DBOut view response = (Event (Partial view), Event (Delta view), Event response)

--data Database query view request response
-- Such that: every Delta query triggers a Partial view; every request (by anyone) triggers a Delta view; every request (by us) triggers a response
database :: DBIn -> m DBOut

times :: DBIn -> DBIn -> m (DBOut, DBOut)

item :: DBOut dbView dbResponse -> DBIn appQuery appResponse -> m (DBIn dbQuery dbRequest, DBOut appView appResponse)

data IV dbquery db dbreq request query result = forall state. IV
  { _iv_notify
    :: query
    -> Delta db --TODO: Should this be NonEmptyDelta, where NonEmptyDelta (a, b) = These (NonEmptyDelta a) (NonEmptyDelta b)
    -> state
    -> (state, Delta result)
  , _iv_subscribe :: Delta query -> db -> (state, result)
  , _iv_request :: request -> db -> (dbreq, a)
  }

timesIV :: IV db request query result -> IV db2 request2 query2 result2 -> IV (db, db2) (Either request request2) (query, query2) (result, result2)
timesIV (IV n s {- r -}) (IV n2 s2 {- r2 -}) = IV
  (\(q, q2) (ddb, ddb2) (s, s2) -> _ (n q ddb s, n2 q2 ddb2 s2))
  _

composeIV :: IV result (Delta db) query2 result2 -> IV db request query result
-}

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
  gDatabasePatchDecoder = M1 $ K1 patchEntity

type family FieldPatches be x where
  FieldPatches be (D1 f x) = D1 f (FieldPatches be x)
  FieldPatches be (C1 f x) = C1 f (FieldPatches be x)
  FieldPatches be (x :*: y) = FieldPatches be x :*: FieldPatches be y
  FieldPatches be (S1 f (K1 Generic.R (Exposed a))) = S1 f (K1 Generic.R (FieldDecoder be a))
  FieldPatches be (S1 f (K1 Generic.R (tbl Exposed))) = S1 f (K1 Generic.R (tbl (FieldDecoder be)))
  FieldPatches be (S1 f (K1 Generic.R (tbl (Nullable Exposed)))) = S1 f (K1 Generic.R (tbl (Nullable (FieldDecoder be))))

class GPatchFields be exposedRep where
  gPatchFields :: FieldPatches be exposedRep ()

instance GPatchFields be x => GPatchFields be (D1 f x) where
  gPatchFields = M1 $ gPatchFields @be @x

instance GPatchFields be x => GPatchFields be (C1 f x) where
  gPatchFields = M1 $ gPatchFields @be @x

instance (GPatchFields be x, GPatchFields be y) => GPatchFields be (x :*: y) where
  gPatchFields = gPatchFields @be @x :*: gPatchFields @be @y

instance BackendFromField be a => GPatchFields be (S1 f (K1 Generic.R (Exposed a))) where
  gPatchFields = M1 $ K1 FieldDecoder

instance ( GPatchFields be (Rep (tbl Exposed))
         , FieldPatches be (Rep (tbl Exposed)) ~ Rep (tbl (FieldDecoder be))
         , Generic (tbl (FieldDecoder be))
         ) => GPatchFields be (S1 f (K1 Generic.R (tbl Exposed))) where
  gPatchFields = M1 $ K1 $ to' $ gPatchFields @be @(Rep (tbl Exposed))

instance ( GPatchFields be (Rep (tbl (Nullable Exposed)))
         , FieldPatches be (Rep (tbl (Nullable Exposed))) ~ Rep (tbl (Nullable (FieldDecoder be)))
         , Generic (tbl (Nullable (FieldDecoder be)))
         ) => GPatchFields be (S1 f (K1 Generic.R (tbl (Nullable Exposed)))) where
  gPatchFields = M1 $ K1 $ to' $ gPatchFields @be @(Rep (tbl (Nullable Exposed)))

class PatchEntity be e where
  patchEntity :: EntityPatchDecoder be e

from' :: Generic x => x -> Rep x ()
from' = Generic.from

to' :: Generic x => Rep x () -> x
to' = Generic.to

instance ( Generic (tbl (FieldDecoder be))
         , GPatchFields be (Rep (tbl Exposed))
         , FieldPatches be (Rep (tbl Exposed)) ~ Rep (tbl (FieldDecoder be))
         , Table tbl
         , Ord (PrimaryKey tbl Identity)
         , Eq (tbl Maybe)
         ) => PatchEntity be (TableEntity tbl) where
  patchEntity = EntityPatchDecoder_Table $ to' $ gPatchFields @be @(Rep (tbl Exposed))

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

availableValues :: PatchMapWithMove2 k p -> OldMap.Map k (PatchTarget p)
availableValues (PatchMapWithMove2 m) = flip OldMap.mapMaybe m $ \case
  NodeInfo { _nodeInfo_from = From_Insert v } -> Just v
  _ -> Nothing

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

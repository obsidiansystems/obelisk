{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backend.Schema where

import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Migrate.Simple
import Database.Beam.Migrate
import Database.Beam.Postgres

data TaskT f = Task
  { _task_id :: Columnar f Int64
  , _task_description :: Columnar f Text
  , _task_completed :: Columnar f Bool
  }
  deriving (Generic)
type Task = TaskT Identity

deriving instance Show Task
deriving instance Eq Task

instance Beamable TaskT

instance Table TaskT where
  data PrimaryKey TaskT f = TaskId (Columnar f Int64) deriving (Generic)
  primaryKey = TaskId . _task_id

instance Beamable (PrimaryKey TaskT)

data Db f = Db
  { _db_tasks :: f (TableEntity TaskT)
  }
  deriving (Generic)

deriving instance Show (f (TableEntity TaskT)) => Show (Db f)

instance Database Postgres Db

checkedDb :: CheckedDatabaseSettings Postgres Db
checkedDb = defaultMigratableDbSettings

db :: DatabaseSettings Postgres Db
db = unCheckDatabase checkedDb

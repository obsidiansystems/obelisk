{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Backend.Schema where

import Common.Schema

import Database.Beam
import Database.Beam.Migrate.Simple
import Database.Beam.Migrate
import Database.Beam.Postgres

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

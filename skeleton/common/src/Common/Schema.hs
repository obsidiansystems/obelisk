{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
module Common.Schema where

import Data.Aeson
import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.Types

data TaskT f = Task
  { _task_id :: Columnar f (SqlSerial Int64)
  , _task_description :: Columnar f Text
  , _task_completed :: Columnar f Bool
  }
  deriving (Generic)
type Task = TaskT Identity

deriving instance (Show (Columnar f (SqlSerial Int64)), Show (Columnar f Text), Show (Columnar f Bool)) => Show (TaskT f)
deriving instance (Read (Columnar f (SqlSerial Int64)), Read (Columnar f Text), Read (Columnar f Bool)) => Read (TaskT f)
deriving instance (Eq (Columnar f (SqlSerial Int64)), Eq (Columnar f Text), Eq (Columnar f Bool)) => Eq (TaskT f)
deriving instance (Ord (Columnar f (SqlSerial Int64)), Ord (Columnar f Text), Ord (Columnar f Bool)) => Ord (TaskT f)

instance Beamable TaskT

instance Table TaskT where
  newtype PrimaryKey TaskT f = TaskId (Columnar f (SqlSerial Int64))
    deriving (Generic)
  primaryKey = TaskId . _task_id

deriving instance Show (Columnar f (SqlSerial Int64)) => Show (PrimaryKey TaskT f)

deriving instance ToJSONKey a => ToJSONKey (SqlSerial a)
deriving instance FromJSONKey a => FromJSONKey (SqlSerial a)

deriving instance Eq (PrimaryKey TaskT Identity)
deriving instance Ord (PrimaryKey TaskT Identity)
deriving instance ToJSON (PrimaryKey TaskT Identity)
deriving instance FromJSON (PrimaryKey TaskT Identity)
deriving instance ToJSONKey (PrimaryKey TaskT Identity)
deriving instance FromJSONKey (PrimaryKey TaskT Identity)
instance ToJSON (TaskT Identity)
instance FromJSON (TaskT Identity)

instance Beamable (PrimaryKey TaskT)

data Db f = Db
  { _db_tasks :: f (TableEntity TaskT)
  }
  deriving (Generic)

deriving instance Show (f (TableEntity TaskT)) => Show (Db f)

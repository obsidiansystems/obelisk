{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Data.Constraint
import Data.Int
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.Types

data TaskT f = Task
  { _task_id :: Columnar f (SqlSerial Int64)
  , _task_description :: Columnar f Text
  , _task_completed :: Columnar f Bool
  , _task_assignee :: PrimaryKey UserT (Nullable f)
  }
  deriving (Generic)
type Task = TaskT Identity

type AllTaskColumns (c :: * -> Constraint) f = (c (Columnar f (SqlSerial Int64)), c (Columnar f Text), c (Columnar f Bool), c (PrimaryKey UserT (Nullable f)))

deriving instance AllTaskColumns Show f => Show (TaskT f)
deriving instance AllTaskColumns Read f => Read (TaskT f)
deriving instance AllTaskColumns Eq f => Eq (TaskT f)
deriving instance AllTaskColumns Ord f => Ord (TaskT f)

instance Beamable TaskT

instance Table TaskT where
  newtype PrimaryKey TaskT f = TaskId (Columnar f (SqlSerial Int64))
    deriving (Generic)
  primaryKey = TaskId . _task_id

deriving instance Show (Columnar f (SqlSerial Int64)) => Show (PrimaryKey TaskT f)

deriving instance Eq (PrimaryKey TaskT Identity)
deriving instance Ord (PrimaryKey TaskT Identity)
deriving instance ToJSON (PrimaryKey TaskT Identity)
deriving instance FromJSON (PrimaryKey TaskT Identity)
deriving instance ToJSONKey (PrimaryKey TaskT Identity)
deriving instance FromJSONKey (PrimaryKey TaskT Identity)
instance ToJSON (TaskT Identity)
instance FromJSON (TaskT Identity)

instance Beamable (PrimaryKey TaskT)

data UserT f = User
  { _user_id :: Columnar f (SqlSerial Int64)
  , _user_name :: Columnar f Text
  }
  deriving (Generic)
type User = UserT Identity

type AllUserColumns (c :: * -> Constraint) f = (c (Columnar f (SqlSerial Int64)), c (Columnar f Text))

deriving instance AllUserColumns Show f => Show (UserT f)
deriving instance AllUserColumns Read f => Read (UserT f)
deriving instance AllUserColumns Eq f => Eq (UserT f)
deriving instance AllUserColumns Ord f => Ord (UserT f)

instance Beamable UserT

instance Table UserT where
  newtype PrimaryKey UserT f = UserId (Columnar f (SqlSerial Int64))
    deriving (Generic)
  primaryKey = UserId . _user_id

deriving instance Show (Columnar f (SqlSerial Int64)) => Show (PrimaryKey UserT f)

deriving instance Eq (Columnar f (SqlSerial Int64)) => Eq (PrimaryKey UserT f)
deriving instance Ord (Columnar f (SqlSerial Int64)) => Ord (PrimaryKey UserT f)
deriving instance ToJSON (Columnar f (SqlSerial Int64)) => ToJSON (PrimaryKey UserT f)
deriving instance FromJSON (Columnar f (SqlSerial Int64)) => FromJSON (PrimaryKey UserT f)
deriving instance ToJSONKey (Columnar f (SqlSerial Int64)) => ToJSONKey (PrimaryKey UserT f)
deriving instance FromJSONKey (Columnar f (SqlSerial Int64)) => FromJSONKey (PrimaryKey UserT f)
instance ToJSON (UserT Identity)
instance FromJSON (UserT Identity)

instance Beamable (PrimaryKey UserT)

deriving instance ToJSONKey a => ToJSONKey (SqlSerial a)
deriving instance FromJSONKey a => FromJSONKey (SqlSerial a)

data Db f = Db
  { _db_tasks :: f (TableEntity TaskT)
  , _db_users :: f (TableEntity UserT)
  }
  deriving (Generic)

deriving instance (Show (f (TableEntity TaskT)), Show (f (TableEntity UserT))) => Show (Db f)

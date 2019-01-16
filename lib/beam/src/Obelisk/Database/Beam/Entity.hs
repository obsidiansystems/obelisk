{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Database.Beam.Entity where

import Control.Lens
import Data.Text
import Database.Beam
import Database.Beam.Migrate

-- | Associates a database table's value (non-key) component with its key type.
type family Key (value :: (* -> *) -> *) :: ((* -> *) -> *)

-- | Combines a value type and its key type to form a complete database row type.
data Entity (value :: (* -> *) -> *) (f :: * -> *) = Entity
  { _entity_key :: (Key value) f
  , _entity_value :: value f
  } deriving Generic

makeLenses ''Entity

deriving instance (Beamable (Key value), Beamable value) => Beamable (Entity value)
deriving instance (Eq (Key value Identity), Eq (value Identity)) => Eq (Entity value Identity)
deriving instance (Ord (Key value Identity), Ord (value Identity)) => Ord (Entity value Identity)
deriving instance (Show (Key value Identity), Show (value Identity)) => Show (Entity value Identity)
deriving instance (Read (Key value Identity), Read (value Identity)) => Read (Entity value Identity)

instance (Typeable (Key value), Beamable (Key value), Typeable value, Beamable value) => Table (Entity value) where
  data PrimaryKey (Entity value) f = EntityKey (Key value f) deriving Generic
  primaryKey = EntityKey . _entity_key

instance (Typeable (Key value), Beamable (Key value), Typeable value, Beamable value) => Beamable (PrimaryKey (Entity value))

-- | Gets the 'PrimaryKey' of an 'Entity' and switches the argument order for
-- consistency with 'Columnar'.
type ForeignKey f value = PrimaryKey (Entity value) f

-- | A single-column primary key.
newtype Id k f = Id { getId :: Columnar f k }
  deriving (Generic, Beamable)

deriving instance Eq k => Eq (Id k Identity)
deriving instance Ord k => Ord (Id k Identity)
deriving instance Show k => Show (Id k Identity)
deriving instance Read k => Read (Id k Identity)

checkedEntity
  :: Text -- ^ The table name in the schema.
  -> (Key e) (CheckedFieldModification (Entity e))
  -> e (CheckedFieldModification (Entity e))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (Entity e))
checkedEntity name key value = modifyCheckedTable (const name) $ Entity
  { _entity_key = key
  , _entity_value = value
  }

-- | Construct a 'CheckedDatabaseEntity' for an 'Entity' table with a
-- single-column 'Id'-based primary key which will be named "id" in the schema.
checkedEntityWithId
  :: Key e ~ Id a
  => Text
  -> e (CheckedFieldModification (Entity e))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (Entity e))
checkedEntityWithId name value = modifyCheckedTable (const name) $ Entity
  { _entity_key = Id $ checkedFieldNamed $ pack "id"
  , _entity_value = value
  }

-- | Construct a 'CheckedFieldModification' for a foreign key to an 'Entity'
-- table.
checkedEntityForeignKeyFieldNamed
  :: Key value ~ Id a
  => Text
  -> PrimaryKey (Entity value) (CheckedFieldModification (Entity e))
checkedEntityForeignKeyFieldNamed = EntityKey . Id . checkedFieldNamed

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Database.Beam.Entity where

import Control.Category (Category(..))
import Control.Lens
import Control.Monad.Except (MonadError)
import Data.Text
import Database.Beam
import Database.Beam.Migrate
import Prelude hiding (id, (.))

import Obelisk.Route

-- | Associates a database table's value (non-key) component with its key type.
type family KeyT (value :: (* -> *) -> *) :: ((* -> *) -> *)

type Key value = KeyT value Identity

-- | Combines a value type and its key type to form a complete database row type.
data EntityT (value :: (* -> *) -> *) (f :: * -> *) = Entity
  { _entity_key :: KeyT value f
  , _entity_value :: value f
  } deriving Generic

makeLenses ''EntityT

type Entity value = EntityT value Identity

deriving instance (Beamable (KeyT value), Beamable value) => Beamable (EntityT value)
deriving instance (Eq (KeyT value f), Eq (value f)) => Eq (EntityT value f)
deriving instance (Ord (KeyT value f), Ord (value f)) => Ord (EntityT value f)
deriving instance (Show (KeyT value f), Show (value f)) => Show (EntityT value f)
deriving instance (Read (KeyT value f), Read (value f)) => Read (EntityT value f)

instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value) => Table (EntityT value) where
  data PrimaryKey (EntityT value) f = EntityKey (KeyT value f) deriving Generic
  primaryKey = EntityKey . _entity_key

instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value) => Beamable (PrimaryKey (EntityT value))
deriving instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value, Eq (KeyT value f)) => Eq (EntityKeyT f value)
deriving instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value, Ord (KeyT value f)) => Ord (EntityKeyT f value)
deriving instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value, Show (KeyT value f)) => Show (EntityKeyT f value)
deriving instance (Typeable (KeyT value), Beamable (KeyT value), Typeable value, Beamable value, Read (KeyT value f)) => Read (EntityKeyT f value)

-- | Gets the 'PrimaryKey' of an 'Entity' and switches the argument order for
-- consistency with 'Columnar'.
type EntityKeyT f value = PrimaryKey (EntityT value) f

type EntityKey value = EntityKeyT Identity value

-- | A single-column primary key.
newtype Id k f = Id { getId :: Columnar f k }
  deriving (Generic, Beamable)

deriving instance Eq (Columnar f k) => Eq (Id k f)
deriving instance Ord (Columnar f k) => Ord (Id k f)
deriving instance Show (Columnar f k) => Show (Id k f)
deriving instance Read (Columnar f k) => Read (Id k f)

entityIdKey
  :: KeyT value ~ Id k
  => Iso
       (EntityKeyT f1 value)
       (EntityKeyT f2 value)
       (Columnar f1 k)
       (Columnar f2 k)
entityIdKey = iso
  (\(EntityKey (Id x)) -> x)
  (EntityKey . Id)

unsafeTshowIdKeyEncoder
  :: KeyT v ~ Id k
  => (Show k, Read k)
  => (Applicative check, MonadError Text parse)
  => Encoder check parse (EntityKey v) Text
unsafeTshowIdKeyEncoder = unsafeTshowEncoder . isoEncoder entityIdKey

checkedEntity
  :: Text -- ^ The table name in the schema.
  -> KeyT e (CheckedFieldModification (EntityT e))
  -> e (CheckedFieldModification (EntityT e))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (EntityT e))
checkedEntity name key value = modifyCheckedTable (const name) $ Entity
  { _entity_key = key
  , _entity_value = value
  }

-- | Construct a 'CheckedDatabaseEntity' for an 'Entity' table with a
-- single-column 'Id'-based primary key which will be named "id" in the schema.
checkedEntityWithId
  :: KeyT e ~ Id a
  => Text
  -> e (CheckedFieldModification (EntityT e))
  -> EntityModification (CheckedDatabaseEntity be db) be (TableEntity (EntityT e))
checkedEntityWithId name value = modifyCheckedTable (const name) $ Entity
  { _entity_key = Id $ checkedFieldNamed $ pack "id"
  , _entity_value = value
  }

-- | Construct a 'CheckedFieldModification' for a foreign key to an 'Entity'
-- table.
checkedEntityForeignKeyFieldNamed
  :: KeyT value ~ Id a
  => Text
  -> PrimaryKey (EntityT value) (CheckedFieldModification (EntityT e))
checkedEntityForeignKeyFieldNamed = EntityKey . Id . checkedFieldNamed

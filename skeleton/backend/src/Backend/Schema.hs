{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-} --TODO: Eliminate this
module Backend.Schema where

import Common.Schema

import Control.Lens
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import qualified Data.Text as T
import Database.Beam
import Database.Beam.Schema.Tables
import Database.Beam.Migrate.Simple
import Database.Beam.Migrate
import Database.Beam.Postgres

instance Database Postgres Db

replaceAllFieldNames :: forall be db. Database be db => CheckedDatabaseSettings be db -> CheckedDatabaseSettings be db
replaceAllFieldNames d = runIdentity $ zipTables (Proxy @be) (\_ -> pure . f) d d
  where f :: IsDatabaseEntity be e => CheckedDatabaseEntity be db e -> CheckedDatabaseEntity be db e
        f (CheckedDatabaseEntity descriptor preds) =
          let renameEntity = unChecked . dbEntityName %~ T.drop 1 . T.dropWhile (/= '_')
              renameFields = unChecked %~ withFieldRenamer (renamingFields $ \(x :| _) -> T.drop 1 $ T.dropWhile (/= '_') $ T.drop 1 x)
          in CheckedDatabaseEntity (renameFields $ renameEntity descriptor) preds

checkedDb :: CheckedDatabaseSettings Postgres Db
checkedDb = replaceAllFieldNames defaultMigratableDbSettings

db :: DatabaseSettings Postgres Db
db = unCheckDatabase checkedDb

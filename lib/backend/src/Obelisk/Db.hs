module Obelisk.Db (withDb, withDbUri, withConnectionPool) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool, createPool, destroyAllResources)
import Database.PostgreSQL.Simple as PG
import Gargoyle
import Gargoyle.PostgreSQL.Nix (postgresNix)
import System.Directory (doesFileExist)
import System.FilePath

-- | Connects to the database using information at the given location
withDb
  :: FilePath
  -- ^ The path of either a 'Gargoyle'-wrapped Postgres database or a file
  -- containing the URL of a Postgres database.  If the path does not exist, it
  -- will be created with 'Gargoyle'.
  -> (Pool PG.Connection -> IO a)
  -- ^ An IO action to run while the database is open; the database will remain
  -- open until this action exits.
  -> IO a
withDb dbPath a = withDbUri dbPath $ \dbUri -> withConnectionPool dbUri a

withDbUri :: FilePath -> (ByteString -> IO a) -> IO a
withDbUri dbPath a = do
  dbExists <- doesFileExist dbPath
  if dbExists
    -- use the file contents as the uri for an existing server
    then do
      dbUri <- BS.readFile dbPath
      a dbUri
    -- otherwise assume its a folder for a local database
    else do
      defaultConfig <- postgresNix
      let myConfig = defaultConfig
            { _gargoyle_init = \workDir -> do
                _gargoyle_init defaultConfig workDir
                appendFile (workDir </> "postgresql.conf") "\n# Added by Obelisk.Db\nwal_level = logical\nmax_wal_senders = 10\n"
                appendFile (workDir </> "pg_hba.conf") "\n# Added by Obelisk.Db\nlocal replication postgres trust\n"
            }
      withGargoyle myConfig dbPath a

openDbUri :: ByteString -> IO (Pool PG.Connection)
openDbUri dbUri = do
  let openPostgresql = connectPostgreSQL dbUri
      closePostgresql p = close p
  createPool openPostgresql closePostgresql 1 5 20

withConnectionPool :: ByteString -> (Pool PG.Connection -> IO a) -> IO a
withConnectionPool dbUri = bracket (openDbUri dbUri) destroyAllResources

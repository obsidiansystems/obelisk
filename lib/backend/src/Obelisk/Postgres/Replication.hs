{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Listen to database changes as a replication client
--
-- In order for this to work you need the following settings in postgresql.conf:
--
-- @
-- wal_level = logical
-- max_replication_slots = n (where n > 0)
-- @
module Obelisk.Postgres.Replication
  ( withLogicalDecoding
  , LogicalDecodingOptions (..)
  ) where

import Data.Time
import Data.Default
import Control.Concurrent.Async
import Control.Monad.Except
import Data.Word
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Copy as PG
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Database.PostgreSQL.Simple.Types as PG
import qualified Database.PostgreSQL.Simple.Internal as PG
import Data.Int
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Char

import Obelisk.Postgres.QQ

-- | Start a replication session using logical decoding
withLogicalDecoding
  :: ByteString -- ^ Database URI
  -> PG.Identifier -- ^ Logical decoding plugin
  -> LogicalDecodingOptions -- ^ Options; use `def` for default
  -> (TChan ByteString -> IO a) -- ^ Action to run while replication session is active
  -> IO a
withLogicalDecoding dbUri pluginName opts go = bracket (PQ.connectdb $ dbUri <> "?connect_timeout=0&replication=database") PQ.finish $ \unwrappedConn -> do
  wrappedConn <- newMVar unwrappedConn
  let withConn :: forall a. (PQ.Connection -> IO a) -> IO a
      withConn = withMVar wrappedConn
  pgConn <- PG.Connection
    <$> pure wrappedConn
    <*> newMVar mempty
    <*> newIORef 0
  backendPid <- PQ.backendPID unwrappedConn
  putStrLn $ "withLogicalDecoding.d" <> show backendPid
  let slotName = PG.Identifier $ slotNamePrefix <> T.pack (show backendPid)
  [(_, _, _, _) :: (Text, Text, Maybe Text, Text)] <- uncurry (PG.query pgConn) [sqlQ| CREATE_REPLICATION_SLOT ?slotName TEMPORARY LOGICAL ?pluginName NOEXPORT_SNAPSHOT |]
  withConn $ \conn -> do
    Just escapedSlotName <- PQ.escapeIdentifier conn $ encodeUtf8 $ PG.fromIdentifier slotName
    optionsString <- case _logicalDecodingOptions_pluginOptions opts of
      [] -> pure ""
      pluginOptions -> fmap ((" (" <>) . (<> ")") . BS.intercalate ", ") $ forM pluginOptions $ \(name, mValue) -> do
        Just nameString <- PQ.escapeIdentifier conn $ encodeUtf8 $ PG.fromIdentifier name
        valueString <- case mValue of
          Just value -> do
            Just encodedValue <- PQ.escapeStringConn conn value
            pure $ " " <> encodedValue
          Nothing -> pure ""
        pure $ nameString <> valueString
    Just _ <- PQ.exec conn $ "START_REPLICATION SLOT " <> escapedSlotName <> " LOGICAL 0/0" <> optionsString
    pure ()
  writtenLsn <- newIORef 0
  chan <- newTChanIO
  let get = PG.getCopyData pgConn >>= \case
        PG.CopyOutDone _ -> pure () --TODO: Can this ever actually happen? Maybe on database shutdown
        PG.CopyOutRow bs -> case decodeReplicationDownstreamMessage bs of
          Left e -> print e
          Right msg -> case msg of
            ReplicationDownstreamMessage_Data lsn _ _ payload -> do
              atomicModifyIORef writtenLsn $ \old -> (max old lsn, ())
              atomically $ writeTChan chan payload
            ReplicationDownstreamMessage_Keepalive lsn _ shouldReply -> do
              atomicModifyIORef writtenLsn $ \old -> (max old lsn, ())
              when shouldReply sendStatus
      sendStatus = do
        lsn <- readIORef writtenLsn
        now <- toPostgresTime <$> getCurrentTime
        let msg = ReplicationUpstreamMessage_StandbyStatus lsn lsn lsn now True
        let encodedMessage = encodeReplicationUpstreamMessage msg
        PQ.CopyInOk <- PQ.putCopyData unwrappedConn encodedMessage
        PQ.FlushOk <- PQ.flush unwrappedConn
        pure ()
  withAsync (forever $ sendStatus >> threadDelay (10*1000*1000)) $ \_ ->
    withAsync (forever get) $ \_ -> do
      go chan

data LogicalDecodingOptions = LogicalDecodingOptions
  { _logicalDecodingOptions_pluginOptions :: [(PG.Identifier, Maybe ByteString)]
    -- ^ Options to pass to the logical decoding plugin
  }

instance Default LogicalDecodingOptions where
  def = LogicalDecodingOptions
    { _logicalDecodingOptions_pluginOptions = []
    }

slotNamePrefix :: Text
slotNamePrefix = "obelisk_"

-- | See https://www.postgresql.org/docs/current/static/protocol-replication.html
data ReplicationDownstreamMessage
   = ReplicationDownstreamMessage_Data Int64 Int64 PostgresTime ByteString
   | ReplicationDownstreamMessage_Keepalive Int64 PostgresTime Bool
   deriving (Show, Read, Eq, Ord)

-- | See https://www.postgresql.org/docs/current/static/protocol-replication.html
data ReplicationUpstreamMessage
   = ReplicationUpstreamMessage_StandbyStatus Int64 Int64 Int64 PostgresTime Bool
   | ReplicationUpstreamMessage_HotStandbyStatus PostgresTime Int32 Int32 Int32 Int32
   deriving (Show, Read, Eq, Ord)

--NOTE: we assume we are talking to a little-endian Postgres instance
decodeReplicationDownstreamMessage :: MonadError Text m => ByteString -> m ReplicationDownstreamMessage
decodeReplicationDownstreamMessage bs = do
  let decodeHeader = do
        Binary.get @Word8 >>= \case
          -- 'w'
          119 -> do
            header :: ByteString -> ReplicationDownstreamMessage
              <- ReplicationDownstreamMessage_Data <$> Binary.get <*> Binary.get <*> Binary.get
            pure $ pure . header
          -- 'k'
          107 -> do
            result <- ReplicationDownstreamMessage_Keepalive <$> Binary.get <*> Binary.get <*> Binary.get
            pure $ \remaining -> if BS.null remaining
              then pure result
              else throwError $ "decodeReplicationDownstreamMessage: keepalive message had extra data"
          leadByte -> fail $ "decodeReplicationDownstreamMessage: unexpected leading byte: " <> show (chr $ fromIntegral leadByte)
  case Binary.runGetOrFail decodeHeader $ LBS.fromStrict bs of
    Left (_, offset, e) -> throwError $ T.pack $ "decodeReplicationDownstreamMessage: at " <> show offset <> ": " <> e
    Right (remaining, _, finish) -> finish $ LBS.toStrict remaining

encodeReplicationUpstreamMessage :: ReplicationUpstreamMessage -> ByteString
encodeReplicationUpstreamMessage = LBS.toStrict . Binary.runPut . \case
  ReplicationUpstreamMessage_StandbyStatus written flushed applied time pleaseReply -> do
    Binary.put @Word8 114 -- 'r'
    Binary.put written
    Binary.put flushed
    Binary.put applied
    Binary.put time
    Binary.put pleaseReply
  ReplicationUpstreamMessage_HotStandbyStatus time xmin xminEpoch lowestCatalogXmin lowestCatalogXminEopch -> do
    Binary.put @Word8 104 -- 'h'
    Binary.put time
    Binary.put xmin
    Binary.put xminEpoch
    Binary.put lowestCatalogXmin
    Binary.put lowestCatalogXminEopch

newtype PostgresTime = PostgresTime { unPostgresTime :: Int64 } deriving (Show, Read, Eq, Ord, Binary)

toPostgresTime :: UTCTime -> PostgresTime
toPostgresTime t = PostgresTime $ fromIntegral $
  (toModifiedJulianDay (utctDay t) - toModifiedJulianDay postgresEpoch) * (24*60*60*1000*1000)
  + round (utctDayTime t * (1000*1000))
  where
    postgresEpoch = fromGregorian 2000 1 1

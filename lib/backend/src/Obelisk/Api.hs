{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Api where

import Prelude hiding ((.))

import Obelisk.Api.Pipeline
import Obelisk.Db
import Obelisk.Postgres.Replication
import Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
import Obelisk.Request (Request)

import Control.Category
import Control.Concurrent.Async
import Control.Concurrent.STM (TChan, atomically, readTChan, writeTChan, newTChanIO)
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Monad.Trans (lift)
import Data.Aeson (FromJSON, ToJSON)
import Data.Align
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Default
import Data.Functor.Compose
import Data.Map.Monoidal (MonoidalMap (..))
import Data.Pool (withResource)
import Data.Vessel
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Transaction as PG
import Reflex.Patch (Group)
import Reflex.Query.Class (QueryResult, SelectedCount (..))
import Snap.Core (Snap)

newtype ReadDb a = ReadDb { unReadDb :: ReaderT PG.Connection IO a }
  deriving (Functor, Applicative, Monad)

readTransaction :: PG.Connection -> ReadDb a -> IO a
readTransaction conn (ReadDb r) = PG.withTransactionModeRetry m PG.isSerializationError conn $ runReaderT r conn
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.Serializable
          , PG.readWriteMode = PG.ReadOnly
          }

newtype WriteDb a = WriteDb { unWriteDb :: ReaderT PG.Connection IO a }
  deriving (Functor, Applicative, Monad)

writeTransaction :: PG.Connection -> WriteDb a -> IO a
writeTransaction conn (WriteDb r) = PG.withTransactionModeRetry m PG.isSerializationError conn $ runReaderT r conn
  where m = PG.TransactionMode
          { PG.isolationLevel = PG.Serializable
          , PG.readWriteMode = PG.ReadWrite
          }

unsafeReadDb :: (PG.Connection -> IO a) -> ReadDb a
unsafeReadDb f = ReadDb $ lift . f =<< ask

unsafeWriteDb :: (PG.Connection -> IO a) -> WriteDb a
unsafeWriteDb f = WriteDb $ lift . f =<< ask

withApiHandler
  :: forall v query q0 request a.
     ( View v
     , Group (v (Const SelectedCount))
     , query ~ v (Compose (MonoidalMap ClientKey) (Const SelectedCount))
     , q0 ~ v Proxy
     , QueryResult query ~ v (Compose (MonoidalMap ClientKey) Identity)
     , QueryResult (v (Const SelectedCount)) ~ v Identity
     , Monoid (v (Compose (MonoidalMap ClientKey) (Const SelectedCount)))
     , Monoid (v Identity)
     , ToJSON (v Identity)
     , FromJSON (v Proxy)
     , Request request
     )
  => ByteString -- ^ The URI of the DB to connect to
  -> (forall x. request x -> WriteDb x) -- ^ How to handle an API request
  -> (v Proxy -> Transaction -> ReadDb (v Identity)) -- ^ How to handle a change
  -> (v Proxy -> ReadDb (v Identity)) -- ^ How to handle a query
  -> (Snap () -> IO a)
  -> IO a
withApiHandler dbUri handleApi handleChange handleQuery go = do
  withNonEmptyTransactions dbUri $ \transactions -> do
    withConnectionPool dbUri $ \db -> do
      let onChange :: Transaction -> v (Compose (MonoidalMap ClientKey) (Const SelectedCount)) -> IO (v (Compose (MonoidalMap ClientKey) Identity))
          onChange txn q = withResource db $ \conn -> readTransaction conn $ mapDecomposedV (flip handleChange txn) q
      let pipe :: Pipeline IO (MonoidalMap ClientKey (v (Const SelectedCount))) (v (Compose (MonoidalMap ClientKey) (Const SelectedCount)))
          pipe = queryMorphismPipeline transposeView
      rec (qh, finalizeFeed) <- feedPipeline (onChange <$> atomically (readTChan transactions)) (QueryHandler $ \q -> withResource db $ \conn -> readTransaction conn $ mapDecomposedV handleQuery q) r
          (qh', r) <- unPipeline pipe qh r'
          (r', handleListen) <- connectPipelineToWebsockets "" (\r'' -> withResource db $ \conn -> writeTransaction conn $ handleApi r'') qh'
      go handleListen

deriving instance Ord k => Align (MonoidalMap k)

withNonEmptyTransactions :: ByteString -> (TChan Transaction -> IO a) -> IO a
withNonEmptyTransactions dbUri go = do
  let opts = def
        { _logicalDecodingOptions_pluginOptions = [("skip-empty-xacts", Nothing)]
        }
  withLogicalDecoding dbUri "test_decoding" opts $ \decodedLines -> do
    transactions <- newTChanIO
    processLine <- linesToTransactions
    let processLines = forever $ do
          lineRaw <- atomically $ readTChan decodedLines
          let Right l = parseOnly (line <* endOfInput) lineRaw
          Right mTransaction <- processLine l
          forM_ mTransaction $ \transaction ->
            atomically $ writeTChan transactions transaction
    withAsync processLines $ \a -> link a >> do
      go transactions

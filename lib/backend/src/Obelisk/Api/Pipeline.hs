{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Obelisk.Api.Pipeline where

import Prelude hiding (id, (.))

import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
import Data.Typeable
import GHC.Generics
import Control.Lens (imapM_)
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Constraint (Dict)
import Reflex.Query.Base
import Reflex.Query.Class
import Reflex.Patch
import Reflex.Class
import Control.Category
import Snap.Core (Snap, MonadSnap)
import Data.Text (Text)
import Data.Map.Monoidal (MonoidalMap)
import qualified Data.AppendMap as Map
import Data.MonoidMap (MonoidMap (..), monoidMap)
import Data.Foldable
import Data.IORef
import Debug.Trace
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets.Stream as WS
import Network.WebSockets.Snap
import Control.Exception

data SomeRequest t where
    SomeRequest :: (FromJSON x, ToJSON x) => t x -> SomeRequest t

class Request r where
  requestToJSON :: r a -> Value
  requestParseJSON :: Value -> Parser (SomeRequest r)
  requestResponseToJSON :: r a -> Dict (ToJSON a)
  requestResponseFromJSON :: r a -> Dict (FromJSON a)

instance Request r => FromJSON (SomeRequest r) where
  parseJSON = requestParseJSON

instance Request r => ToJSON (SomeRequest r) where
  toJSON (SomeRequest r) = requestToJSON r

-- | A way for a pipeline to retrieve data
newtype QueryHandler q m = QueryHandler
  { runQueryHandler :: q -> m (QueryResult q) }

-- | A way to push data into a pipeline
newtype Recipient q m = Recipient
  { tellRecipient :: QueryResult q -> m () }

-- | A way of attaching to (and later detaching from) a pipeline
newtype Registrar q = Registrar { unRegistrar :: Recipient q IO -> IO (QueryHandler q IO, IO ()) }

-- | A way of connecting a source of data to a consumer of data. The consumer
-- can pull data from the datasource and the datasource can push data to the
-- consumer.
--
-- q is the consumer side
-- q' is the datasource side
newtype Pipeline m q q' = Pipeline { unPipeline :: QueryHandler q' m -> Recipient q m -> IO (QueryHandler q m, Recipient q' m) }

instance Category (Pipeline m) where
  id = Pipeline $ \qh r -> return (qh, r)
  Pipeline f . Pipeline g = Pipeline $ \qh r -> do
    rec (qh'', r') <- g qh' r
        (qh', r'') <- f qh r'
    return (qh'', r'')

-- | A key used to track particular consumers
newtype ClientKey = ClientKey { unClientKey :: Integer }
  deriving (Eq, Ord, Show)

-- | Produce a multi-client 'Recipient' and a single client 'QueryHandler'
fanQuery
  :: forall k q. (Ord k, Monoid (QueryResult q))
  => (k -> IO (Recipient q IO))
    -- ^ Look up a recipient
  -> QueryHandler (MonoidalMap k q) IO
    -- ^ A 'QueryHandler' for multiple clients
  -> ( Recipient (MonoidalMap k q) IO
       -- Used to notify recipients of new 'QueryResult' data
     , k -> QueryHandler q IO
       -- Used by 'multiplexQuery' to lookup the 'QueryHandler' for a given client
     )
fanQuery lookupRecipient qh = (multiRecipient lookupRecipient, fanQueryHandler qh)
  where
    -- | Given a recipient lookup function, produces a 'Recipient' that can
    -- handle distribution of a map of 'QueryResult's to many recipients
    multiRecipient lookupR = Recipient $ imapM_ $ \k qr -> do
      s <- lookupR k
      tellRecipient s qr
    -- | Given a multi-client 'QueryHandler', produces a 'QueryHandler' for
    -- a single client
    fanQueryHandler qh' = \k -> mapQueryHandler (singletonQuery k) qh'

-- | Maintains a Map from connected clients to a 'Recipient' that can be used to transmit data to the clients
--
-- Takes a function that looks up a the 'QueryHandler' for a given client
-- Returns: 1. A lookup function into the Map of clients
--          2. A way to register a new client
--             a. A 'QueryHandler' for the newly registered client
--             b. A removal callback to de-register a particular client
multiplexQuery
  :: (MonadIO m, Group q)
  => (ClientKey -> QueryHandler q m)
  -> IO ( ClientKey -> IO (Recipient q m)
        , Recipient q m -> IO (QueryHandler q m, m ())
        )
multiplexQuery lookupQueryHandler = do
  clients <- newIORef (ClientKey 0, Map.empty :: MonoidalMap ClientKey (Recipient q m, q))
  let
    lookupRecipient k = do
      (_, cs) <- readIORef clients
      case Map.lookup k cs of
        Nothing -> do
          putStrLn $ mconcat
            [ "Rhyolite.Backend.App.multiplexQuery: Failed to find sender for key"
            , show k
            , " in known keys "
            , show $ Map.keys cs
            ]
          return $ Recipient $ const $ return ()
        Just s -> return $ fst s

    registerRecipient s = do
      cid <- atomicModifyIORef' clients $ \(cid, recipients) ->
        ((ClientKey (unClientKey cid + 1), Map.insert cid (s, mempty) recipients), cid)
      let
        queryHandler = QueryHandler $ \q -> do
          liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            ((nextCid, Map.update (\(r, oldQ) -> Just (r, oldQ <> q)) cid recipients), ())
          runQueryHandler (lookupQueryHandler cid) q

        unregisterRecipient = do
          antiQ <- liftIO $ atomicModifyIORef' clients $ \(nextCid, recipients) ->
            case Map.updateLookupWithKey (\_ _ -> Nothing) cid recipients of
              (Nothing, _) -> trace
                ("Rhyolite.Backend.App.multiplexQuery: Tried to unregister a client key that is not registered " <> show cid)
                ((nextCid, recipients), mempty)
              (Just (_, removedQuery), newRecipients) -> ((nextCid, newRecipients), negateG removedQuery)

          -- TODO: Should we have a way of ensuring that this doesn't actually cause a query to be run?
          -- It shouldn't cause the query to be run again but it depends on if the callee will notice
          -- that the new query is strictly smaller than the old one.
          runQueryHandler (lookupQueryHandler cid) antiQ
          return ()

      return (queryHandler, unregisterRecipient)

  return (lookupRecipient, registerRecipient)

-- | Represents a WebSocket message from one of two channels: ViewSelector declarations or API requests
data WebSocketRequest q r
  = WebSocketRequest_ViewSelector q
  | WebSocketRequest_Api (TaggedRequest r)
  deriving (Typeable, Generic)

instance (Request r, FromJSON q) => FromJSON (WebSocketRequest q r)
instance (Request r, ToJSON q) => ToJSON (WebSocketRequest q r)

-- | Represents a WebSocket response from one of three channels: incoming 'View's, API responses, or version info
data WebSocketResponse q
  = WebSocketResponse_View (QueryResult q)
  | WebSocketResponse_Api TaggedResponse
  | WebSocketResponse_Version Text
  deriving (Typeable, Generic)

instance FromJSON (QueryResult q) => FromJSON (WebSocketResponse q)
instance ToJSON (QueryResult q) => ToJSON (WebSocketResponse q)

-- | A request tagged with an identifier
data TaggedRequest r = TaggedRequest Value (SomeRequest r)
  deriving (Typeable, Generic)

instance Request r => FromJSON (TaggedRequest r)
instance Request r => ToJSON (TaggedRequest r)

-- | A response tagged with an identifier matching the one in the 'TaggedRequest'. The identifier is the first argument.
data TaggedResponse = TaggedResponse Value Value
  deriving (Typeable, Generic)

instance FromJSON TaggedResponse
instance ToJSON TaggedResponse

-- | Handles a websocket connection
handleWebsocket
  :: forall q qr r.
     ( Eq (q SelectedCount)
     , Eq (qr SelectedCount)
     , QueryResult (q SelectedCount) ~ qr SelectedCount
     , QueryResult (q ()) ~ qr ()
     , ToJSON (qr ())
     , Functor qr
     , Group (q SelectedCount)
     , Monoid (qr SelectedCount)
     , FromJSON (q ())
     , Request r
     , Functor q
     )
  => Text -- ^ Version
  -> (forall a. r a -> IO a)
  -> Registrar (q SelectedCount)
  -> Snap ()
handleWebsocket v rh register = withWebsocketsConnection $ \conn -> do
  let sender = Recipient $ sendEncodedDataMessage conn . (\a -> WebSocketResponse_View (void a) :: WebSocketResponse (q ()))
  sendEncodedDataMessage conn (WebSocketResponse_Version v :: WebSocketResponse (q ()))
  bracket (unRegistrar register sender) snd $ \(vsHandler, _) -> flip evalStateT mempty $ forever $ do
    (wsr :: WebSocketRequest (q ()) r) <- liftIO $ getDataMessage conn
    case wsr of
      WebSocketRequest_Api (TaggedRequest reqId (SomeRequest req)) -> lift $ do
        a <- rh req
        sendEncodedDataMessage conn
          (WebSocketResponse_Api $ TaggedResponse reqId (toJSON a) :: WebSocketResponse (q ()))
      WebSocketRequest_ViewSelector new -> do
        old <- get
        let new' = SelectedCount 1 <$ new
            vsDiff = new' ~~ old
        when (vsDiff /= mempty) $ do
          qr <- lift $ runQueryHandler vsHandler vsDiff
          put new'
          when (qr /= mempty) $ lift $
            sendEncodedDataMessage conn (WebSocketResponse_View (void qr) :: WebSocketResponse (q ()))

-------------------------------------------------------------------------------

-- | Connect a datasource (e.g., a database) to a pipeline
--
-- Data taken from 'getNextNotification' is pushed into the pipeline and
-- when the pipeline pulls data, it is retrieved using 'qh'
feedPipeline
  :: Monoid q
  => IO (q -> IO (QueryResult q))
  -- ^ Get the next notification to be sent to the pipeline. If no notification
  -- is available, this should block until one is available
  -> QueryHandler q IO
  -- ^ Retrieve data when requested by pipeline
  -> Recipient q IO
  -- ^ A way to push data into the pipeline
  -> IO (QueryHandler q IO, IO ())
  -- ^ A way for the pipeline to request data
feedPipeline getNextNotification qh r = do
  currentQuery <- newIORef mempty
  let qhSaveQuery = QueryHandler $ \q -> do
        atomicModifyIORef' currentQuery $ \old -> (q <> old, ())
        runQueryHandler qh q
  tid <- forkIO . forever $ do
    nm <- getNextNotification
    q <- readIORef currentQuery
    qr <- nm q
    tellRecipient r qr
  return (qhSaveQuery, killThread tid)

-- | Connects the pipeline to websockets consumers
connectPipelineToWebsockets
  :: ( Eq (q SelectedCount)
     , QueryResult (q SelectedCount) ~ qr SelectedCount
     , QueryResult (q ()) ~ qr ()
     , Group (q SelectedCount)
     , Monoid (qr SelectedCount)
     , Eq (qr SelectedCount)
     , ToJSON (qr ())
     , FromJSON (q ())
     , Request r
     , Functor qr
     , Functor q
     )
  => Text
  -> (forall a. r a -> IO a)
  -- ^ API handler
  -> QueryHandler (MonoidalMap ClientKey (q SelectedCount)) IO
  -- ^ A way to retrieve more data for each consumer
  -> IO (Recipient (MonoidalMap ClientKey (q SelectedCount)) IO, Snap ())
  -- ^ A way to send data to many consumers and a handler for websockets connections
connectPipelineToWebsockets ver rh qh = do
  (allRecipients, registerRecipient) <- connectPipelineToWebsockets' qh
  return (allRecipients, handleWebsocket ver rh registerRecipient)

-- | Like 'connectPipelineToWebsockets' but returns a Registrar that can
-- be used to construct a handler for a particular client
connectPipelineToWebsockets'
  :: (Monoid (QueryResult q), Group q)
  => QueryHandler (MonoidalMap ClientKey q) IO
  -> IO (Recipient (MonoidalMap ClientKey q) IO, Registrar q)
  -- ^ A way to send data to many consumers, and a way to register new consumers
connectPipelineToWebsockets' qh = do
  rec (lookupRecipient, registerRecipient) <- multiplexQuery clientQueryHandler
      let (allRecipients, clientQueryHandler) = fanQuery lookupRecipient qh
  return (allRecipients, Registrar registerRecipient)

-- | Extends a 'Registrar' with a 'Pipeline'
extendRegistrar :: Pipeline IO q q' -> Registrar q' -> Registrar q
extendRegistrar (Pipeline p) (Registrar r) = Registrar $ \recipient -> do
  rec (qh', close) <- r recipient'
      (qh, recipient') <- p qh' recipient
  return (qh, close)

-------------------------------------------------------------------------------

singletonQuery
  :: ( Monoid (QueryResult q)
     , Ord k
     )
  => k
  -> QueryMorphism q (MonoidalMap k q)
singletonQuery k = QueryMorphism { _queryMorphism_mapQuery = Map.singleton k
                                 , _queryMorphism_mapQueryResult = Map.findWithDefault mempty k
                                 }

monoidMapQueryMorphism :: (Eq q, Monoid q) => QueryMorphism (MonoidalMap k q) (MonoidMap k q)
monoidMapQueryMorphism = QueryMorphism
  { _queryMorphism_mapQuery = monoidMap
  , _queryMorphism_mapQueryResult = unMonoidMap
  }

transposeMonoidMap
  :: forall a a' k q qr.
     ( Ord k
     , Eq a
     , Monoid a
     , Monoid a'
     , Eq (q (MonoidMap k a))
     , Eq (QueryResult (q a))
     , Foldable qr
     , Functor q
     , Functor qr
     , FunctorMaybe qr
     , Monoid (q (MonoidMap k a))
     , Monoid (QueryResult (q a))
     , QueryResult (q (MonoidMap k a)) ~ qr (MonoidMap k a')
     , QueryResult (q a) ~ qr a'
     )
  => QueryMorphism (MonoidMap k (q a)) (q (MonoidMap k a))
transposeMonoidMap = QueryMorphism
  { _queryMorphism_mapQuery = aggregateQueries
  , _queryMorphism_mapQueryResult = distributeResults
  }
  where
    aggregateQueries :: MonoidMap k (q a) -> q (MonoidMap k a)
    aggregateQueries = fold . monoidMap . Map.mapWithKey (\k q -> fmap (monoidMap . Map.singleton k) q) . unMonoidMap
    distributeResults :: qr (MonoidMap k a') -> MonoidMap k (qr a')
    distributeResults v = monoidMap $ Map.mapWithKey (\k _ -> fmapMaybe (Map.lookup k . unMonoidMap) v) $ fold $ fmap unMonoidMap v

mapQueryHandlerAndRecipient
  :: Functor f
  => QueryMorphism q q'
  -> QueryHandler q' f
  -> Recipient q f
  -> (QueryHandler q f, Recipient q' f)
mapQueryHandlerAndRecipient qm qh s = (mapQueryHandler qm qh, mapRecipient qm s)

mapQueryHandler :: Functor f => QueryMorphism q q' -> QueryHandler q' f -> QueryHandler q f
mapQueryHandler qm qh = QueryHandler $ \qs -> mapQueryResult qm <$> runQueryHandler qh (mapQuery qm qs)

mapRecipient :: QueryMorphism q q' -> Recipient q m -> Recipient q' m
mapRecipient qm s = Recipient $ \qr -> tellRecipient s $ mapQueryResult qm qr

fmapQueryMorphism
  :: ( Functor f, QueryResult (f q) ~ f (QueryResult q)
     , QueryResult (f q') ~ f (QueryResult q') )
  => QueryMorphism q q'
  -> QueryMorphism (f q) (f q')
fmapQueryMorphism qm = QueryMorphism
  { _queryMorphism_mapQuery = fmap $ _queryMorphism_mapQuery qm
  , _queryMorphism_mapQueryResult = fmap $ _queryMorphism_mapQueryResult qm
  }

queryMorphismPipeline :: Functor m => QueryMorphism q q' -> Pipeline m q q'
queryMorphismPipeline qm = Pipeline $ \qh r -> pure $ mapQueryHandlerAndRecipient qm qh r

-- | Accepts a websockets connection and runs the supplied action with it
withWebsocketsConnection :: MonadSnap m => (WS.Connection -> IO ()) -> m ()
withWebsocketsConnection f = runWebSocketsSnap $ \pc -> do
  conn <- WS.acceptRequest pc
  handleSomeException $ handleConnectionException pc $ f conn
  where
    handleSomeException = handle $ \(SomeException e) -> putStrLn $ "withWebsocketsConnection: " <> displayException e
    handleConnectionException pc = handle $ \e -> case e of
      WS.ConnectionClosed -> return ()
      WS.CloseRequest _ _ -> print e >> WS.close (WS.pendingStream pc) >> throwIO e
      _ -> do putStr $ "withWebsocketsConnection: Exception: " <> displayException e
              throwIO e

-- | Attempts to json decode a websockets data message
decodeWebsocketsDataMessage :: FromJSON a => WS.DataMessage -> Either String a
decodeWebsocketsDataMessage dm = eitherDecode' $ case dm of
  WS.Text r' _ -> r'
  WS.Binary r' -> r'

-- | Parse and process a single websocket data message
getDataMessage
  :: FromJSON a
  => WS.Connection
  -> IO a
getDataMessage conn = do
  dm <- WS.receiveDataMessage conn
  case decodeWebsocketsDataMessage dm of
    Left err -> liftIO $ throwIO $ AssertionFailed $ mconcat
      [ "getDataMessage: error: "
      , err
      , "; received: "
      , show dm
      ]
    Right a -> return a

-- | Send a json encoded data message over the websocket connection
sendEncodedDataMessage
  :: ToJSON a
  => WS.Connection
  -> a
  -> IO ()
sendEncodedDataMessage conn = WS.sendDataMessage conn . (\x -> WS.Text x Nothing) . encode

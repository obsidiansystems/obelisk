module Obelisk.Postgres.Notify.Big (withBigNotificationListener) where

createSpillTable :: PG.Connection -> PG.Identifier -> IO ()
createSpillTable conn spillTableName = do
  void $ uncurry (PG.execute conn) [sqlQ| CREATE TABLE ?spillTableName (id bigserial PRIMARY KEY, value bytea NOT NULL) |]
    --TODO: Drop things from this table when they're done being consumed

-- | The largest size of payload that can be sent via Postgres's NOTIFY command
-- See https://www.postgresql.org/docs/current/static/sql-notify.html
maxSmallNotificationSize :: Int
maxSmallNotificationSize = 8000

pattern SmallMessageMarker :: Word8
pattern SmallMessageMarker = 83 -- 'S'

pattern LargeMessageMarker :: Word8
pattern LargeMessageMarker = 76 -- 'L'

data Message
   = Message_Small ByteString -- This ByteString, plus one additional byte, must fit into a Postgres NOTIFY message
   | Message_Large Int64 -- Key in the spill table where the message can be found

-- We can't write a Binary instance for this, because that would require us to store the length inside the ByteString we're encoding to
encodeMessage :: Message -> ByteString
encodeMessage = \case
  Message_Small rawMsg -> BS.cons SmallMessageMarker rawMsg
  Message_Large messageId -> BS.cons LargeMessageMarker $ LBS.toStrict $ Binary.encode messageId

decodeMessage :: ByteString -> Either (LBS.ByteString, ByteOffset, String) Message
decodeMessage bs = case BS.uncons bs of
  Nothing -> Left (LBS.empty, 0, "decodeMessage: input is empty")
  Just (h, t) -> case h of
    SmallMessageMarker -> Right $ Message_Small t
    LargeMessageMarker -> case Binary.decodeOrFail $ LBS.fromStrict t of
      Right (remaining, offset, messageId) -> if LBS.null remaining
        then Right $ Message_Large messageId
        else Left (remaining, offset + 1, "decodeMessage: extra input remaining after decoding")
      Left (remaining, offset, e) -> Left (remaining, offset + 1, "decodeMessage: error decoding message ID: " <> e)

sendBigNotification :: PG.Connection -> PG.Identifier -> PG.Identifier -> ByteString -> IO ()
sendBigNotification conn channelName spillTableName rawMessage = do
  packedMessage <- packBigNotification conn spillTableName rawMessage
  _ <- uncurry (PG.execute conn) [sqlQ| NOTIFY ?packedMessage |]
  pure ()

-- | Turn a ByteString of any size (up to the maximum size of a Postgres `bytea` value) into a ByteString small enough to be sent using NOTIFY
packBigNotification :: PG.Connection -> PG.Identifier -> ByteString -> IO ByteString
packBigNotification conn spillTableName rawMessage = do
  preparedMessage <- if BS.length rawMessage <= maxSmallNotificationSize - 1
    then pure $ Message_Small rawMessage
    else do
      [Only messageId] <- uncurry (PG.query conn) [sqlQ| INSERT INTO ?spillTableName (value) VALUES (?rawMessage) RETURNING id |]
      pure $ Message_Large messageId
  pure $ encodeMessage preparedMessage

-- | Unpack a ByteString created by 'packBigNotification'
unpackBigNotification :: PG.Connection -> PG.Identifier -> ByteString -> IO ByteString
unpackBigNotification conn spillTableName packedMessage = do
  preparedMessage <- case decodeMessage packedMessage of
    Right preparedMessage -> pure preparedMessage
    Left (remaining, offset, e) -> fail $ "unpackBigNotification: error unpacking notification at offset " <> show offset <> " with " <> show remaining <> " byte(s) left: " <> e
  case preparedMessage of
    Message_Small rawMessage -> pure rawMessage
    Message_Large messageId -> do
      [Only rawMessage] <- uncurry (PG.query conn) [sqlQ| SELECT value FROM ?spillTableName WHERE id = ?messageId |]
      pure rawMessage

withBigNotificationListener :: Pool PG.Connection -> PG.Identifier -> PG.Identifier -> (ByteString -> IO ()) -> IO a -> IO a
withBigNotificationListener db channelName spillTableName onMessage = withNotificationListener db channelName $ \packedMessage -> do
  rawMessage <- withResource db $ \conn -> do
    unpackBigNotification conn spillTableName packedMessage
  onMessage rawMessage


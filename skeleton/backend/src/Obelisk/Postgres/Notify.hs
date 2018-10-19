module Obelisk.Postgres.Notify (withNotificationListener) where

unning.
-- The message callback is called synchronously, so the next message cannot be received until it returns
withNotificationListener :: Pool PG.Connection -> PG.Identifier -> (ByteString -> IO ()) -> IO a -> IO a
withNotificationListener db channelName onMessage = withThread_ $ withResource db $ \conn -> do
  _ <- uncurry (PG.execute conn) [sqlQ| LISTEN ?channelName |]
  forever $ do
    PG.Notification _ channel message <- PG.getNotification conn
    if channel == encodeUtf8 (PG.fromIdentifier channelName)
      then onMessage message `catchAny` \e -> putStrLn $ "withNotificationListener: " <> show e --TODO: catchAny prevents problems when a listener throws an exception, but not if it hangs indefinitely.  Perhaps we should instead use a Chan or TChan to get values out
      else putStrLn $ "withNotificationListener: Received a message on unexpected channel: " <> show channel

withThread_ :: IO () -> IO b -> IO b
withThread_ t go = bracket (forkIO t) killThread $ \_ -> go

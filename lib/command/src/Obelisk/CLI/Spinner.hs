{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.CLI.Spinner where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T

import Obelisk.CLI.Logging (LoggingConfig, Output (..), handleLog)

-- | Run an action with a CLI spinner. You should use the more convenient
-- `Obelisk.Command.Utils.withSpinner` instead.
withSpinner'
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => LoggingConfig -> Text -> m a -> m a
withSpinner' c s = bracket run cleanup . const
  where
    -- TODO: Can we obviate passing LoggingConfig just to use forkIO?
    run = liftIO $ forkIO $ flip runLoggingT (handleLog c) $ do
      forM_ (cycle spinner1) $ \e -> do
        logMessage $ Output_Raw ["\r", [e], " ", T.unpack s]
        delay
    cleanup tid = do
      liftIO $ killThread tid
      logMessage Output_ClearLine
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.

spinner1 :: String
spinner1 = "◐◓◑◒"

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | An extension of `System.Process` that integrates with logging (`Obelisk.CLI.Logging`)
-- and is thus spinner friendly.
module Obelisk.CLI.Process
  ( readProcessAndLogStderr
  , callProcessAndLogOutput
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog)
import Control.Monad.Reader (MonadIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Streams (InputStream, handleToInputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Concurrent (concurrentMerge)
import System.Process (CreateProcess, StdStream (CreatePipe), cmdspec, createProcess, std_err, std_out,
                       waitForProcess)

import Obelisk.CLI.Logging (Output, Severity (..), failWith, putLogRaw)

-- | Like `System.Process.readProcess` but logs the stderr instead of letting the external process inherit it.
readProcessAndLogStderr
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => Severity -> CreateProcess -> m String
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog sev =<< liftIO (handleToInputStream err)
  -- TODO: Why are we using Text here?
  liftIO $ T.unpack . T.strip <$> T.hGetContents out

-- | Like `System.Process.callProcess` but logs the combined output (stdout and stderr)
callProcessAndLogOutput
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => Severity -> CreateProcess -> m ()
callProcessAndLogOutput sev process = do
  void $ withProcess process $ \out err -> do
    stream <- liftIO $ do
      uncurry combineStream =<< liftA2 (,) (handleToInputStream out) (handleToInputStream err)
    streamToLog sev stream
  where
    combineStream s1 s2 = concurrentMerge [s1, s2]

withProcess
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => CreateProcess -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process action = do
  (_, Just out, Just err, p) <- liftIO $ createProcess $ process
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  action out err  -- Pass the handles to the custom action
  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()
    ExitFailure code -> do
      -- Log an error. We also fail immediately; however we should probably let the caller control that.
      failWith $ T.pack $ show (cmdspec process) <> " failed with exit code: " <> show code
  return (out, err)  -- Return the handles

-- | Read from an input stream and log its contents
streamToLog
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => Severity -> InputStream BSC.ByteString -> m ()
streamToLog sev stream = fix $ \loop -> do
  liftIO (Streams.read stream) >>= \case
    Nothing -> return ()
    Just line -> putLogRaw sev (decodeUtf8 line) >> loop

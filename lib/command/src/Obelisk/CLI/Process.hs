{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | An extension of `System.Process` that integrates with logging (`Obelisk.CLI.Logging`)
-- and is thus spinner friendly.
module Obelisk.CLI.Process
  ( readProcessAndLogStderr
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  ) where

import Control.Applicative (liftA2)
import Control.Monad (void, (<=<))
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog)
import Control.Monad.Reader (MonadIO)
import qualified Data.ByteString.Char8 as BSC
import Data.Function (fix)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Streams (InputStream, handleToInputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Concurrent (concurrentMerge)
import System.Process (CreateProcess, ProcessHandle, StdStream (CreatePipe), cmdspec, std_err, std_out,
                       waitForProcess)
import qualified System.Process as Process

import Obelisk.CLI.Logging (Output, Severity (..), failWith, putLog, putLogRaw)

-- | Like `System.Process.readProcess` but logs the stderr instead of letting the external process inherit it.
readProcessAndLogStderr
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => Severity -> CreateProcess -> m String
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  -- TODO: Why are we using Text here?
  liftIO $ T.unpack . T.strip <$> T.hGetContents out

-- | Like `System.Process.callProcess` but logs the combined output--stdout and stderr with the corresponding
-- severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However some processes
-- are known to spit out diagnostic or informative messages in stderr, in which case it is advisable to call
-- it with a non-Error severity for stderr, like `callProcessAndLogOutput (Debug, Debug)`.
callProcessAndLogOutput
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => (Severity, Severity) -> CreateProcess -> m ()
callProcessAndLogOutput (sev_out, sev_err) process = do
  void $ withProcess process $ \out err -> do
    stream <- liftIO $ do
      uncurry combineStream =<< liftA2 (,) (streamHandle sev_out out) (streamHandle sev_err err)
    streamToLog stream
  where
    combineStream s1 s2 = concurrentMerge [s1, s2]

-- | Like `System.Process.createProcess` but also logs (debug) the process being run
createProcess
  :: (MonadIO m, MonadLog Output m)
  => CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
  putLog Debug $ "Creating process: " <> T.pack (show p)
  liftIO $ Process.createProcess p

-- | Like `System.Process.createProcess_` but also logs (debug) the process being run
createProcess_
  :: (MonadIO m, MonadLog Output m)
  => String -> CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ name p = do
  putLog Debug $ "Creating process " <> T.pack name <> ": " <> T.pack (show p)
  liftIO $ Process.createProcess p


withProcess
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => CreateProcess -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process f = do
  let procTitle = T.pack $ show $ cmdspec process
  (_, Just out, Just err, p) <- createProcess $ process
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  f out err  -- Pass the handles to the passed function
  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return ()
    ExitFailure code -> do
      -- Log an error. We also fail immediately; however we should probably let the caller control that.
      failWith $ procTitle <> " failed with exit code: " <> T.pack (show code)
  return (out, err)  -- Return the handles

-- Create an input stream from the file handle, associating each item with the given severity.
streamHandle :: Severity -> Handle -> IO (InputStream (Severity, BSC.ByteString))
streamHandle sev = Streams.map (sev,) <=< handleToInputStream

-- | Read from an input stream and log its contents
streamToLog
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => InputStream (Severity, BSC.ByteString) -> m ()
streamToLog stream = fix $ \loop -> do
  liftIO (Streams.read stream) >>= \case
    Nothing -> return ()
    Just (sev, line) -> putLogRaw sev (decodeUtf8 line) >> loop

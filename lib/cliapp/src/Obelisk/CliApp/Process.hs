{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | An extension of `System.Process` that integrates with logging (`Obelisk.CLI.Logging`)
-- and is thus spinner friendly.
module Obelisk.CliApp.Process
  ( ProcessFailed (..)
  , readProcessAndLogStderr
  , readProcessAndLogOutput
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  , callProcess
  , callCommand
  ) where

import Control.Monad ((<=<), join, void)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (fix)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
--import qualified Data.Text.IO as T
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Streams (InputStream, handleToInputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Concurrent (concurrentMerge)
import System.Process (CreateProcess, ProcessHandle, StdStream (CreatePipe), cmdspec, std_err, std_out,
                       waitForProcess)
import qualified System.Process as Process

import Control.Monad.Log (Severity (..))
import Obelisk.CliApp.Logging (putLog, putLogRaw)
import Obelisk.CliApp.Types (Cli, ProcessFailed (..), reconstructCommand)

readProcessAndLogStderr
  :: (MonadIO m, Cli m)
  => Severity -> CreateProcess -> m Text
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  liftIO $ T.decodeUtf8 <$> BS.hGetContents out

-- | Like `System.Process.readProcess` but logs the combined output (stdout and stderr)
-- with the corresponding severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However
-- some processes are known to spit out diagnostic or informative messages in stderr, in
-- which case it is advisable to call it with a non-Error severity for stderr, like
-- `callProcessAndLogOutput (Debug, Debug)`.

readProcessAndLogOutput
  :: (MonadIO m, Cli m)
  => (Severity, Severity) -> CreateProcess -> m Text
readProcessAndLogOutput (sev_out, sev_err) process = do
  (_, Just out, Just err, p) <- createProcess $ process
    { std_out = CreatePipe , std_err = CreatePipe }

  -- TODO interleave stdout and stderr in log correctly
  streamToLog =<< liftIO (streamHandle sev_err err)
  outText <- liftIO $ T.decodeUtf8 <$> BS.hGetContents out
  putLogRaw sev_out outText

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> pure outText
    ExitFailure code -> throwError $ Right $ ProcessFailed (cmdspec process) code

-- | Like `System.Process.callProcess` but logs the combined output (stdout and stderr)
-- with the corresponding severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However
-- some processes are known to spit out diagnostic or informative messages in stderr, in
-- which case it is advisable to call it with a non-Error severity for stderr, like
-- `callProcessAndLogOutput (Debug, Debug)`.
callProcessAndLogOutput

  :: (MonadIO m, Cli m)
  => (Severity, Severity) -> CreateProcess -> m ()
callProcessAndLogOutput (sev_out, sev_err) process =
  void $ withProcess process $ \out err -> do
    stream <- liftIO $ join $ combineStream
      <$> streamHandle sev_out out
      <*> streamHandle sev_err err
    streamToLog stream
  where
    combineStream s1 s2 = concurrentMerge [s1, s2]

-- | Like `System.Process.createProcess` but also logs (debug) the process being run
createProcess
  :: (MonadIO m, Cli m)
  => CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
  putLog Debug $ "Creating process: " <> reconstructCommand (cmdspec p)
  liftIO $ Process.createProcess p

-- | Like `System.Process.createProcess_` but also logs (debug) the process being run
createProcess_
  :: (MonadIO m, Cli m)
  => String -> CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ name p = do
  putLog Debug $ "Creating process " <> T.pack name <> ": " <> reconstructCommand (cmdspec p)
  liftIO $ Process.createProcess p

-- | Like `System.Process.callProcess` but also logs (debug) the process being run
callProcess
  :: (MonadIO m, Cli m)
  => String -> [String] -> m ()
callProcess exe args = do
  putLog Debug $ "Calling process " <> T.pack exe <> " with args: " <> T.pack (show args)
  liftIO $ Process.callProcess exe args

-- | Like `System.Process.callCommand` but also logs (debug) the command being run
callCommand
  :: (MonadIO m, Cli m)
  => String -> m ()
callCommand cmd = do
  putLog Debug $ "Calling command " <> T.pack cmd
  liftIO $ Process.callCommand cmd

withProcess
  :: (MonadIO m, Cli m)
  => CreateProcess -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process f = do -- TODO: Use bracket.
  -- FIXME: Using `withCreateProcess` here leads to something operating illegally on closed handles.
  (_, Just out, Just err, p) <- createProcess $ process
    { std_out = CreatePipe , std_err = CreatePipe }

  f out err  -- Pass the handles to the passed function

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return (out, err)
    ExitFailure code -> throwError $ Right $ ProcessFailed (cmdspec process) code

-- Create an input stream from the file handle, associating each item with the given severity.
streamHandle :: Severity -> Handle -> IO (InputStream (Severity, BSC.ByteString))
streamHandle sev = Streams.map (sev,) <=< handleToInputStream

-- | Read from an input stream and log its contents
streamToLog
  :: (MonadIO m, Cli m)
  => InputStream (Severity, BSC.ByteString) -> m ()
streamToLog stream = fix $ \loop -> do
  liftIO (Streams.read stream) >>= \case
    Nothing -> return ()
    Just (sev, line) -> putLogRaw sev (T.decodeUtf8 line) >> loop

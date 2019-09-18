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
  ( ProcessFailure (..)
  , AsProcessFailure (..)
  , readProcessAndLogStderr
  , readProcessAndLogOutput
  , readCreateProcessWithExitCode
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  , callProcess
  , callCommand
  , reconstructCommand
  , shellQuoteAndEscapeDouble
  , shellQuoteAndEscapeSingle
  ) where

import Control.Monad ((<=<), join, void)
import Control.Monad.Except (throwError)
import Control.Monad.Fail
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (Prism', review)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (fix)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (lenientDecode)
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
import Obelisk.CliApp.Types (CliLog, CliThrow)

-- TODO put back in `Obelisk.CliApp.Process` and use prisms for extensible exceptions
data ProcessFailure = ProcessFailure Process.CmdSpec Int -- exit code
  deriving Show

-- | Indicates arbitrary process failures form one variant (or conceptual projection) of
-- the error type.
class AsProcessFailure e where
  asProcessFailure :: Prism' e ProcessFailure

instance AsProcessFailure ProcessFailure where
  asProcessFailure = id

readProcessAndLogStderr
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => Severity -> CreateProcess -> m Text
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out

readCreateProcessWithExitCode
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e)
  => CreateProcess -> m (ExitCode, String, String)
readCreateProcessWithExitCode process = do
  putLog Debug $ "Creating process: " <> reconstructCommand (cmdspec process)
  liftIO $ Process.readCreateProcessWithExitCode process ""

-- | Like `System.Process.readProcess` but logs the combined output (stdout and stderr)
-- with the corresponding severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However
-- some processes are known to spit out diagnostic or informative messages in stderr, in
-- which case it is advisable to call it with a non-Error severity for stderr, like
-- `callProcessAndLogOutput (Debug, Debug)`.
readProcessAndLogOutput
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => (Severity, Severity) -> CreateProcess -> m Text
readProcessAndLogOutput (sev_out, sev_err) process = do
  (_, Just out, Just err, p) <- createProcess $ process
    { std_out = CreatePipe , std_err = CreatePipe }

  -- TODO interleave stdout and stderr in log correctly
  streamToLog =<< liftIO (streamHandle sev_err err)
  outText <- liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out
  putLogRaw sev_out outText

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> pure outText
    ExitFailure code -> throwError $ review asProcessFailure $ ProcessFailure (cmdspec process) code

-- | Like `System.Process.callProcess` but logs the combined output (stdout and stderr)
-- with the corresponding severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However
-- some processes are known to spit out diagnostic or informative messages in stderr, in
-- which case it is advisable to call it with a non-Error severity for stderr, like
-- `callProcessAndLogOutput (Debug, Debug)`.
callProcessAndLogOutput

  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
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
  :: (MonadIO m, CliLog m)
  => CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess p = do
  putLog Debug $ "Creating process: " <> reconstructCommand (cmdspec p)
  liftIO $ Process.createProcess p

-- | Like `System.Process.createProcess_` but also logs (debug) the process being run
createProcess_
  :: (MonadIO m, CliLog m)
  => String -> CreateProcess -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ name p = do
  putLog Debug $ "Creating process " <> T.pack name <> ": " <> reconstructCommand (cmdspec p)
  liftIO $ Process.createProcess p

-- | Like `System.Process.callProcess` but also logs (debug) the process being run
callProcess
  :: (MonadIO m, CliLog m)
  => String -> [String] -> m ()
callProcess exe args = do
  putLog Debug $ "Calling process " <> T.pack exe <> " with args: " <> T.pack (show args)
  liftIO $ Process.callProcess exe args

-- | Like `System.Process.callCommand` but also logs (debug) the command being run
callCommand
  :: (MonadIO m, CliLog m)
  => String -> m ()
callCommand cmd = do
  putLog Debug $ "Calling command " <> T.pack cmd
  liftIO $ Process.callCommand cmd

withProcess
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => CreateProcess -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process f = do -- TODO: Use bracket.
  -- FIXME: Using `withCreateProcess` here leads to something operating illegally on closed handles.
  (_, Just out, Just err, p) <- createProcess $ process
    { std_out = CreatePipe , std_err = CreatePipe }

  f out err  -- Pass the handles to the passed function

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return (out, err)
    ExitFailure code -> throwError $ review asProcessFailure $ ProcessFailure (cmdspec process) code

-- Create an input stream from the file handle, associating each item with the given severity.
streamHandle :: Severity -> Handle -> IO (InputStream (Severity, BSC.ByteString))
streamHandle sev = Streams.map (sev,) <=< handleToInputStream

-- | Read from an input stream and log its contents
streamToLog
  :: (MonadIO m, CliLog m)
  => InputStream (Severity, BSC.ByteString) -> m ()
streamToLog stream = fix $ \loop -> do
  liftIO (Streams.read stream) >>= \case
    Nothing -> return ()
    Just (sev, line) -> putLogRaw sev (T.decodeUtf8With lenientDecode line) >> loop

-- | Pretty print a 'CmdSpec'
reconstructCommand :: Process.CmdSpec -> Text
reconstructCommand (Process.ShellCommand str) = T.pack str
reconstructCommand (Process.RawCommand c as) = processToShellString c as
  where
    processToShellString cmd args = T.unwords $ map (shellQuoteAndEscapeSingle . T.pack) (cmd : args)

shellQuoteAndEscapeSingle :: Text -> Text
shellQuoteAndEscapeSingle x = "'" <> T.replace "'" "'\''" x <> "'"

shellQuoteAndEscapeDouble :: Text -> Text
shellQuoteAndEscapeDouble x = "\"" <> T.replace "\"" "\\\"" x <> "\""

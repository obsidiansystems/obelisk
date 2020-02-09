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
  ( AsProcessFailure (..)
  , ProcessFailure (..)
  , ProcessSpec (..)
  , callCommand
  , callProcess
  , callProcessAndLogOutput
  , createProcess
  , createProcess_
  , overCreateProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessAndLogOutput
  , readProcessAndLogStderr
  , readProcessJSONAndLogStderr
  , reconstructCommand
  , setCwd
  , setDelegateCtlc
  , setEnvOverride
  , shell
  ) where

import Control.Monad ((<=<), join, void)
import Control.Monad.Except (throwError)
import Control.Monad.Fail
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens (Prism', review)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (fix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.IO (Handle)
import System.IO.Streams (InputStream, handleToInputStream)
import qualified System.IO.Streams as Streams
import System.IO.Streams.Concurrent (concurrentMerge)
import System.Process (CreateProcess, ProcessHandle, StdStream (CreatePipe), std_err, std_out, waitForProcess)
import qualified System.Process as Process
import qualified Data.Aeson as Aeson

import Control.Monad.Log (Severity (..))
import Obelisk.CliApp.Logging (putLog, putLogRaw)
import Obelisk.CliApp.Types (CliLog, CliThrow)

data ProcessSpec = ProcessSpec
  { _processSpec_createProcess :: !CreateProcess
  , _processSpec_overrideEnv :: !(Maybe (Map String String -> Map String String))
  }

proc :: FilePath -> [String] -> ProcessSpec
proc cmd args = ProcessSpec (Process.proc cmd args) Nothing

shell :: String -> ProcessSpec
shell cmd = ProcessSpec (Process.shell cmd) Nothing

setEnvOverride :: (Map String String -> Map String String) -> ProcessSpec -> ProcessSpec
setEnvOverride f p = p { _processSpec_overrideEnv = Just f }

overCreateProcess :: (CreateProcess -> CreateProcess) -> ProcessSpec -> ProcessSpec
overCreateProcess f (ProcessSpec p x) = ProcessSpec (f p) x

setDelegateCtlc :: Bool -> ProcessSpec -> ProcessSpec
setDelegateCtlc b = overCreateProcess (\p -> p { Process.delegate_ctlc = b })

setCwd :: Maybe FilePath -> ProcessSpec -> ProcessSpec
setCwd fp = overCreateProcess (\p -> p { Process.cwd = fp })


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
  => Severity -> ProcessSpec -> m Text
readProcessAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out

readProcessJSONAndLogStderr
  :: (Aeson.FromJSON a, MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => Severity -> ProcessSpec -> m a
readProcessJSONAndLogStderr sev process = do
  (out, _err) <- withProcess process $ \_out err -> do
    streamToLog =<< liftIO (streamHandle sev err)
  json <- liftIO $ BS.hGetContents out
  case Aeson.eitherDecodeStrict json of
    Right a -> pure a
    Left err -> do
      putLog Error $ "Could not decode process output as JSON: " <> T.pack err
      throwError $ review asProcessFailure $ ProcessFailure (Process.cmdspec $ _processSpec_createProcess process) 0

readCreateProcessWithExitCode
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e)
  => ProcessSpec -> m (ExitCode, String, String)
readCreateProcessWithExitCode procSpec = do
  process <- mkCreateProcess procSpec
  putLog Debug $ "Creating process: " <> reconstructProcSpec procSpec
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
  => (Severity, Severity) -> ProcessSpec -> m Text
readProcessAndLogOutput (sev_out, sev_err) process = do
  (_, Just out, Just err, p) <- createProcess $ overCreateProcess
    (\p -> p { std_out = CreatePipe , std_err = CreatePipe }) process

  -- TODO interleave stdout and stderr in log correctly
  streamToLog =<< liftIO (streamHandle sev_err err)
  outText <- liftIO $ T.decodeUtf8With lenientDecode <$> BS.hGetContents out
  putLogRaw sev_out outText

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> pure outText
    ExitFailure code -> throwError $ review asProcessFailure $ ProcessFailure (Process.cmdspec $ _processSpec_createProcess process) code

-- | Like 'System.Process.callProcess' but logs the combined output (stdout and stderr)
-- with the corresponding severity.
--
-- Usually this function is called as `callProcessAndLogOutput (Debug, Error)`. However
-- some processes are known to spit out diagnostic or informative messages in stderr, in
-- which case it is advisable to call it with a non-Error severity for stderr, like
-- `callProcessAndLogOutput (Debug, Debug)`.
callProcessAndLogOutput
  :: (MonadIO m, CliLog m, CliThrow e m, AsProcessFailure e, MonadFail m)
  => (Severity, Severity) -> ProcessSpec -> m ()
callProcessAndLogOutput (sev_out, sev_err) process =
  void $ withProcess process $ \out err -> do
    stream <- liftIO $ join $ combineStream
      <$> streamHandle sev_out out
      <*> streamHandle sev_err err
    streamToLog stream
  where
    combineStream s1 s2 = concurrentMerge [s1, s2]

-- | Like 'System.Process.createProcess' but also logs (debug) the process being run
createProcess
  :: (MonadIO m, CliLog m)
  => ProcessSpec -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess procSpec = do
  p <- mkCreateProcess procSpec
  putLog Debug $ "Creating process: " <> reconstructProcSpec procSpec
  liftIO $ Process.createProcess p

-- | Like `System.Process.createProcess_` but also logs (debug) the process being run
createProcess_
  :: (MonadIO m, CliLog m)
  => String -> ProcessSpec -> m (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess_ name procSpec = do
  p <- mkCreateProcess procSpec
  putLog Debug $ "Creating process " <> T.pack name <> ": " <> reconstructProcSpec procSpec
  liftIO $ Process.createProcess_ name p

mkCreateProcess :: MonadIO m => ProcessSpec -> m Process.CreateProcess
mkCreateProcess (ProcessSpec p override') = do
  case override' of
    Nothing -> pure p
    Just override -> do
      procEnv <- Map.fromList <$> maybe (liftIO getEnvironment) pure (Process.env p)
      pure $ p { Process.env = Just $ Map.toAscList (override procEnv) }

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
  => ProcessSpec -> (Handle -> Handle -> m ()) -> m (Handle, Handle)
withProcess process f = do -- TODO: Use bracket.
  -- FIXME: Using `withCreateProcess` here leads to something operating illegally on closed handles.
  (_, Just out, Just err, p) <- createProcess $ overCreateProcess
    (\x -> x { std_out = CreatePipe , std_err = CreatePipe }) process

  f out err  -- Pass the handles to the passed function

  liftIO (waitForProcess p) >>= \case
    ExitSuccess -> return (out, err)
    ExitFailure code -> throwError $ review asProcessFailure $ ProcessFailure (Process.cmdspec $ _processSpec_createProcess process) code

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
reconstructCommand p = case p of
  Process.ShellCommand str -> T.pack str
  Process.RawCommand c as -> processToShellString c as
  where
    processToShellString cmd args = T.unwords $ map quoteAndEscape (cmd : args)
    quoteAndEscape x = "'" <> T.replace "'" "'\''" (T.pack x) <> "'"

reconstructProcSpec :: ProcessSpec -> Text
reconstructProcSpec = reconstructCommand . Process.cmdspec . _processSpec_createProcess

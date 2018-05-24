{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.CLI.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT (LoggingT), MonadLog, Severity (..), WithSeverity (..), runLoggingT)
import Control.Monad.Reader (MonadIO, ReaderT (..), ask)
import Data.IORef (IORef)
import Data.Text (Text)
import UnliftIO (MonadUnliftIO, UnliftIO (..), askUnliftIO, withUnliftIO)


data CliConfig = CliConfig
  { _cliConfig_logLevel :: IORef Severity  -- We are capable of changing the log level at runtime
  , _cliConfig_noColor :: Bool  -- Disallow coloured output
  , _cliConfig_noSpinner :: Bool  -- Disallow spinners
  , _cliConfig_lock :: MVar Bool  -- Whether the last message was an Overwrite output
  , _cliConfig_tipDisplayed :: IORef Bool  -- Whether the user tip (to make verbose) was already displayed
  , _cliConfig_spinnerStack :: IORef [Text] -- Stack of logs from nested spinners
  }

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Overwrite [String]  -- Overwrites the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clears the line
  deriving (Eq, Show, Ord)

type Cli m = MonadLog Output m

newtype CliT m a = CliT
  { unCliT :: ReaderT CliConfig (LoggingT Output m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output
    )

class Monad m => HasCliConfig m where
  getCliConfig :: m CliConfig

instance Monad m => HasCliConfig (CliT m) where
  getCliConfig = CliT ask

instance MonadUnliftIO m => MonadUnliftIO (LoggingT Output m) where
  askUnliftIO = LoggingT $ ReaderT $ \f ->
    withUnliftIO $ \u ->
      return (UnliftIO (unliftIO u . flip runLoggingT f))

instance MonadUnliftIO m => MonadUnliftIO (CliT m) where
  askUnliftIO = CliT $ withUnliftIO $ \u ->
    return (UnliftIO (unliftIO u . unCliT))

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.CliApp.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT, MonadLog, Severity (..), WithSeverity (..))
import Control.Monad.Reader (MonadIO, ReaderT (..), ask)
import Data.IORef (IORef)
import Data.Text (Text)

import Obelisk.CliApp.TerminalString (TerminalString)

data CliConfig = CliConfig
  { _cliConfig_logLevel :: IORef Severity  -- We are capable of changing the log level at runtime
  , _cliConfig_noColor :: Bool  -- Disallow coloured output
  , _cliConfig_noSpinner :: Bool  -- Disallow spinners
  , _cliConfig_lock :: MVar Bool  -- Whether the last message was an Overwrite output
  , _cliConfig_tipDisplayed :: IORef Bool  -- Whether the user tip (to make verbose) was already displayed
  , _cliConfig_spinnerStack :: IORef [TerminalString] -- Stack of logs from nested spinners
  }

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Write [TerminalString]  -- Render and write a TerminalString using putstrLn
  | Output_Overwrite [TerminalString]  -- Overwrite the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clear the line
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

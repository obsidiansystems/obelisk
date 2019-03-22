{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.CliApp.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT, MonadLog, Severity (..), WithSeverity (..), logMessage)
import Control.Monad.Reader (MonadIO, ReaderT (..), MonadReader (..), ask, mapReaderT)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.IORef (IORef)
import Data.Text (Text)
import System.Exit (ExitCode (..), exitWith)

import Obelisk.CliApp.TerminalString (TerminalString)
import Obelisk.CliApp.Theme (CliTheme)

--------------------------------------------------------------------------------

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Write [TerminalString]  -- Render and write a TerminalString using putstrLn
  | Output_Overwrite [TerminalString]  -- Overwrite the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clear the line
  deriving (Eq, Show, Ord)

type CliLog m = MonadLog Output m

type CliThrow e m = MonadError e m

-- | Log a message to the console.
--
-- Logs safely even if there are ongoing spinners.
putLog :: CliLog m => Severity -> Text -> m ()
putLog sev = logMessage . Output_Log . WithSeverity sev

newtype DieT e m a = DieT { unDieT :: ReaderT (e -> (Text, Int)) (LoggingT Output m) a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output
    )

instance MonadTrans (DieT e) where
  lift = DieT . lift . lift

-- | Error printer is private to DieT
instance MonadReader r m => MonadReader r (DieT e m) where
  ask = DieT $ lift $ ask
  local = (\f (DieT a) -> DieT $ f a) . mapReaderT . local
  reader = DieT . lift . lift . reader

-- TODO generalize to bigger error types
instance MonadIO m => MonadError e (DieT e m) where
  throwError e = do
    handler <- DieT ask
    let (output, exitCode) = handler e
    putLog Alert output
    liftIO $ exitWith $ ExitFailure exitCode

  -- Cannot catch
  catchError m _ = m

--------------------------------------------------------------------------------

data CliConfig e = CliConfig
  { -- | We are capable of changing the log level at runtime
    _cliConfig_logLevel :: IORef Severity
  , -- | Disallow coloured output
    _cliConfig_noColor :: Bool
  , -- | Disallow spinners
    _cliConfig_noSpinner :: Bool
  , -- | Whether the last message was an Overwrite output
    _cliConfig_lock :: MVar Bool
  , -- | Whether the user tip (to make verbose) was already displayed
    _cliConfig_tipDisplayed :: IORef Bool
  , -- | Stack of logs from nested spinners
    _cliConfig_spinnerStack :: IORef ([Bool], [TerminalString])
  , -- | Failure handler. How to log error and what exit status to use.
    _cliConfig_errorLogExitCode :: e -> (Text, Int)
  , -- | Theme strings for spinners
    _cliConfig_theme :: CliTheme
  }

class Monad m => HasCliConfig e m | m -> e where
  getCliConfig :: m (CliConfig e)

instance HasCliConfig e m => HasCliConfig e (ReaderT r m) where
  getCliConfig = lift getCliConfig

instance (Monoid w, HasCliConfig e m) => HasCliConfig e (WriterT w m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig e m => HasCliConfig e (StateT s m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig e m => HasCliConfig e (ExceptT e m) where
  getCliConfig = lift getCliConfig

--------------------------------------------------------------------------------

newtype CliT e m a = CliT
  { unCliT :: ReaderT (CliConfig e) (DieT e m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output -- CliLog
    , MonadError e -- CliThrow
    , MonadReader (CliConfig e) -- HasCliConfig
    )

instance MonadTrans (CliT e) where
  lift = CliT . lift . lift

instance Monad m => HasCliConfig e (CliT e m)where
  getCliConfig = ask

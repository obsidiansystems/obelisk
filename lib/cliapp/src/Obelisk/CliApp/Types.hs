{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.CliApp.Types where

import Control.Concurrent.MVar (MVar)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Log (LoggingT, MonadLog, Severity (..), WithSeverity (..), logMessage)
import Control.Monad.Reader (MonadIO, ReaderT (..), MonadReader, ask)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (StateT)
import Control.Monad.Except (ExceptT, MonadError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadTrans, lift)
import Data.IORef (IORef)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode (..), exitWith)
import qualified System.Process as Process

import Obelisk.CliApp.TerminalString (TerminalString)

--------------------------------------------------------------------------------

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_LogRaw (WithSeverity Text)  -- Like `Output_Log` but without the implicit newline added.
  | Output_Write [TerminalString]  -- Render and write a TerminalString using putstrLn
  | Output_Overwrite [TerminalString]  -- Overwrite the current line (i.e. \r followed by `putStr`)
  | Output_ClearLine  -- Clear the line
  deriving (Eq, Show, Ord)

type CliLog m = MonadLog Output m

type CliThrow m = MonadError (Either Text ProcessFailed) m

-- TODO could remove for granularity
type Cli m = (CliLog m, CliThrow m)

-- | Log a message to the console.
--
-- Logs safely even if there are ongoing spinners.
putLog :: Cli m => Severity -> Text -> m ()
putLog sev = logMessage . Output_Log . WithSeverity sev

-- TODO put back in `Obelisk.CliApp.Process` and use prisms for extensible exceptions
data ProcessFailed = ProcessFailed Process.CmdSpec Int -- exit code
  deriving Show

-- TODO put back in `Obelisk.CliApp.Process`
reconstructCommand :: Process.CmdSpec -> Text
reconstructCommand (Process.ShellCommand str) = T.pack str
reconstructCommand (Process.RawCommand c as) = processToShellString c as
  where
    processToShellString cmd args = T.unwords $ map quoteAndEscape (cmd : args)
    quoteAndEscape x = "'" <> T.replace "'" "'\''" (T.pack x) <> "'"


newtype DieT m a = DieT { unDieT :: LoggingT Output m a }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader r
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output
    )

instance MonadTrans DieT where
  lift = DieT . lift

-- TODO generalize to bigger error types
instance MonadIO m => MonadError (Either Text ProcessFailed) (DieT m) where
  throwError = \case
    Left s -> do
      putLog Alert s
      liftIO $ exitWith $ ExitFailure 2
    Right (ProcessFailed p code) -> do
      putLog Alert $ "Process exited with code " <> T.pack (show code) <> "; " <> T.pack (show p)
      liftIO $ exitWith $ ExitFailure 2

  -- Cannot catch
  catchError m _ = m

--------------------------------------------------------------------------------

data CliConfig = CliConfig
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
  }

class Monad m => HasCliConfig m where
  getCliConfig :: m CliConfig

instance HasCliConfig m => HasCliConfig (ReaderT r m) where
  getCliConfig = lift getCliConfig

instance (Monoid w, HasCliConfig m) => HasCliConfig (WriterT w m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig m => HasCliConfig (StateT s m) where
  getCliConfig = lift getCliConfig

instance HasCliConfig m => HasCliConfig (ExceptT e m) where
  getCliConfig = lift getCliConfig

--------------------------------------------------------------------------------

newtype CliT m a = CliT
  { unCliT :: ReaderT CliConfig (DieT m) a
  }
  deriving
    ( Functor, Applicative, Monad, MonadIO
    , MonadThrow, MonadCatch, MonadMask
    , MonadLog Output, MonadError (Either Text ProcessFailed), MonadReader CliConfig
    )

instance MonadTrans CliT where
  lift = CliT . lift . lift

instance Monad m => HasCliConfig (CliT m) where
  getCliConfig = ask

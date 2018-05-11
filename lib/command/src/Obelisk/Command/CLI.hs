{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.Command.CLI where

import Control.Monad.Catch (MonadThrow, finally, throwM)
import Control.Monad.Reader (MonadIO, liftIO, reader)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD (stdout)
import System.Environment (getArgs)
import System.IO (hFlush, hIsTerminalDevice)

import System.Console.ANSI
import System.Console.Questioner (dots1Spinner, stopIndicator)

import Obelisk.App (MonadObelisk, _obelisk_verbose)

-- TODO: This doesn't handle the put* line of functions below, in that: when we print anything while the
-- spinner is already running, it won't appear correctly in the terminal. The exception is `failWith` which
-- raises an exception (that gets handled properly here). One solution to fix this problem is to run a
-- singleton spinner thread and interact with it in order to print something.
withSpinner
  :: MonadObelisk m
  => Text  -- ^ Text to print alongside the spinner
  -> Maybe Text  -- ^ Optional text to print at the end
  -> m a  -- ^ Action to run and wait for
  -> m a
withSpinner s e f = do
  verbose <- reader _obelisk_verbose
  isTerm <- liftIO $ hIsTerminalDevice stdout
  -- When running in shell completion, disable the spinner.
  inBashCompletion <- liftIO $ isInfixOf "completion" . unwords <$> getArgs
  let spinnerDisabled = not isTerm || inBashCompletion || verbose
  case spinnerDisabled of
    True -> do
      putInfo s
      f
    False -> do
      spinner <- liftIO $ dots1Spinner (1000 * 200) $ T.unpack s
      result <- finally f $ do
        liftIO $ stopIndicator spinner
      case e of
        Just exitMsg -> putInfo exitMsg
        Nothing -> liftIO $ hFlush stdout
      return result

data Level = Level_Normal | Level_Warning | Level_Error
  deriving (Bounded, Enum, Eq, Ord, Show)

-- TODO: Handle this error cleanly when evaluating outside of `withSpinner` (eg: runCLI)
failWith :: MonadThrow m => Text -> m a
failWith = throwM . userError . T.unpack

putError :: MonadIO m => Text -> m ()
putError = liftIO . putMsg Level_Error

putWarning :: MonadIO m => Text -> m ()
putWarning = liftIO . putMsg Level_Warning

putInfo :: MonadIO m => Text -> m ()
putInfo = liftIO . putMsg Level_Normal

putMsg :: MonadIO m => Level -> Text -> m ()
putMsg level s = liftIO $ do
  setColor level
  finally (T.putStrLn s) $
    setSGR [Reset]

setColor :: Level -> IO ()
setColor = \case
  Level_Error -> do
    setSGR [SetColor Foreground Vivid Red]
    -- setSGR [SetColor Background Vivid White]
  Level_Warning -> do
    setSGR [SetColor Foreground Vivid Yellow]
    -- setSGR [SetColor Background Vivid Black]
  Level_Normal -> return ()

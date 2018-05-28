{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a simple CLI spinner that interoperates cleanly with the rest of the logging output.
module Obelisk.CLI.Spinner (withSpinner) where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.IO.Class
import Control.Monad.Log (logMessage)
import Control.Monad.Reader (MonadIO)
import Data.IORef
import qualified Data.List as L
import Data.Text (Text)
import System.Console.ANSI (Color (Blue, Cyan, Green, Red))

import Obelisk.CLI.Logging (allowUserToMakeLoggingVerbose, fork)
import Obelisk.CLI.TerminalString (TerminalString (..), enquiryCode)
import Obelisk.CLI.Types (Cli, CliConfig (..), HasCliConfig, Output (..), getCliConfig)

-- | Run an action with a CLI spinner.
withSpinner
  :: (MonadIO m, MonadMask m, Cli m, HasCliConfig m)
  => Text -> m a -> m a
withSpinner s = bracket' start cleanup . const
  where
    start = do
      tids <- run
      tid <- fork $ allowUserToMakeLoggingVerbose enquiryCode
      pure $ tid : tids
    run = do
      -- Add this log to the spinner stack
      stack <- _cliConfig_spinnerStack <$> getCliConfig
      wasEmpty <- liftIO $ atomicModifyIORef' stack $ \old -> (TerminalString_Normal s : old, null old)
      if wasEmpty
        then do -- Fork a thread to manage output of anything on the stack
          tid <- fork $ runSpinner spinner $ \c -> do
            logs <- liftIO $ concatStack c <$> readIORef stack
            logMessage $ Output_Overwrite logs
          pure [tid]
        else pure []
    cleanup failed tids = do
      stack <- _cliConfig_spinnerStack <$> getCliConfig
      liftIO $ mapM_ killThread tids
      logMessage Output_ClearLine
      let mark = if failed
            then TerminalString_Colorized Red "✖"
            else TerminalString_Colorized Green "✔"
      -- Delete this log from the spinner stack
      logs <- liftIO $ atomicModifyIORef' stack $ \old -> (L.delete (TerminalString_Normal s) old, concatStack mark old)
      logMessage $ Output_Write logs
    spinner = coloredSpinner defaultSpinnerTheme

-- | How nested spinner logs should be displayed
concatStack :: TerminalString -> [TerminalString] -> [TerminalString]
concatStack mark = L.intersperse space . go . L.reverse
  where
    go [] = []
    go (x:[]) = mark : [x]
    go (x:xs) = arrow : x : go xs
    arrow = TerminalString_Colorized Blue "⇾"
    space = TerminalString_Normal " "

-- | A spinner is simply an infinite list of strings that supplant each other in a delayed loop, creating the
-- animation of a "spinner".
type Spinner = [TerminalString]

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = cycle . fmap (TerminalString_Colorized Cyan)

-- | Run a spinner with a monadic function that defines how to represent the individual spinner characters.
runSpinner :: MonadIO m => Spinner -> (TerminalString -> m ()) -> m ()
runSpinner spinner f = forM_ spinner $ f >=> const delay
  where
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.

type SpinnerTheme = [Text]

-- Find more spinners at https://github.com/sindresorhus/cli-spinners/blob/master/spinners.json
spinnerCircleHalves :: SpinnerTheme
spinnerCircleHalves = ["◐", "◓", "◑", "◒"]

defaultSpinnerTheme :: SpinnerTheme
defaultSpinnerTheme = spinnerCircleHalves

-- | Like `bracket` but the `release` function can know whether an exception was raised
bracket' :: MonadMask m => m a -> (Bool -> a -> m b) -> (a -> m c) -> m c
bracket' acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release True resource
  _ <- release False resource
  return result

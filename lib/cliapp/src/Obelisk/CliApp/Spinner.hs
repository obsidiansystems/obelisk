{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Provides a simple CLI spinner that interoperates cleanly with the rest of the logging output.
module Obelisk.CliApp.Spinner
  ( withSpinner
  , withSpinner'
  ) where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.IO.Class
import Control.Monad.Log (Severity (..), logMessage)
import Control.Monad.Reader (MonadIO)
import Data.IORef
import qualified Data.List as L
import Data.Text (Text)
import System.Console.ANSI (Color (Blue, Cyan, Green, Red))

import Obelisk.CliApp.Logging (allowUserToMakeLoggingVerbose, fork, putLog)
import Obelisk.CliApp.TerminalString (TerminalString (..), enquiryCode)
import Obelisk.CliApp.Types (Cli, CliConfig (..), HasCliConfig, Output (..), getCliConfig)

-- | Run an action with a CLI spinner.
withSpinner
  :: (MonadIO m, MonadMask m, Cli m, HasCliConfig m)
  => Text -> m a -> m a
withSpinner s = withSpinner' (s, const $ Just s)

-- | Advanced version that controls the last spinner output based on action's result.
withSpinner'
  :: (MonadIO m, MonadMask m, Cli m, HasCliConfig m)
  => (Text, a -> Maybe Text) -- ^ How to (and whether to) render the final spinner message; note that this doesn't take sub-spinner behaviour into account (although it probably should)
  -> m a
  -> m a
withSpinner' (s, s') f = do
  noSpinner <- _cliConfig_noSpinner <$> getCliConfig
  if noSpinner
    then putLog Notice s >> f
    else bracket' run cleanup $ const f
  where
    run = do
      -- Add this log to the spinner stack
      stack <- _cliConfig_spinnerStack <$> getCliConfig
      wasEmpty <- liftIO $ atomicModifyIORef' stack $ \old -> (TerminalString_Normal s : old, null old)
      if wasEmpty
        then do -- Fork a thread to manage output of anything on the stack
          ctrleThread <- fork $ allowUserToMakeLoggingVerbose enquiryCode
          spinnerThread <- fork $ runSpinner spinner $ \c -> do
            logs <- liftIO $ concatStack c <$> readIORef stack
            logMessage $ Output_Overwrite logs
          pure [ctrleThread, spinnerThread]
        else pure []
    cleanup tids resultM = do
      stack <- _cliConfig_spinnerStack <$> getCliConfig
      liftIO $ mapM_ killThread tids
      logMessage Output_ClearLine
      let (mark, msgM) = case resultM of
            Nothing -> (TerminalString_Colorized Red "✖", Just s)
            Just result -> (TerminalString_Colorized Green "✔", s' result)
      logsM <- liftIO $ atomicModifyIORef' stack $ \old ->
        let
          new = L.delete (TerminalString_Normal s) old
        in
          (new, (\msg -> concatStack mark $ TerminalString_Normal msg : new) <$> msgM)
      forM_ logsM $ logMessage . Output_Write
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
bracket' :: MonadMask m => m a -> (a -> Maybe c -> m b) -> (a -> m c) -> m c
bracket' acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release resource Nothing
  _ <- release resource $ Just result
  return result

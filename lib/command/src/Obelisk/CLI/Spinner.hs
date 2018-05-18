{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a simple CLI spinner that interoperates cleanly with the rest of the logging output.
module Obelisk.CLI.Spinner (withSpinner') where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.Log (MonadLog, logMessage)
import Control.Monad.Reader (MonadIO)
import Control.Monad.IO.Class
import Data.IORef
import Data.List (delete)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (Color (Cyan, Green, Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), setSGRCode)

import Obelisk.CLI.Logging (LoggingConfig(..), Output (..), allowUserToMakeLoggingVerbose, forkML)

-- | Run an action with a CLI spinner.
withSpinner'
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => LoggingConfig -> Text -> m a -> m a
withSpinner' conf s = bracket' start cleanup . const
  where
    start = do
      tids <- run
      tid <- forkML conf $ allowUserToMakeLoggingVerbose conf enquiryCode
      pure $ tid : tids
    stack = _loggingConfig_stack conf
    run = do
      -- Add this log to the spinner stack
      wasEmpty <- liftIO $ atomicModifyIORef' stack $ \old -> (s : old, null old)
      if wasEmpty
        then do -- Fork a thread to manage output of anything on the stack
          tid <- forkML conf $ runSpinner spinner $ \c -> do
            logs <- liftIO $ concatStack c <$> readIORef stack
            logMessage $ Output_Overwrite [logs]
          pure [tid]
        else pure []
    cleanup failed tids = do
      liftIO $ mapM_ killThread tids
      logMessage Output_ClearLine
      let mark = if failed
            then withColor Red "✖"
            else withColor Green "✔"
      -- Delete this log from the spinner stack
      logs <- liftIO $ atomicModifyIORef' stack $ \old -> (delete s old, concatStack mark old)
      logMessage $ Output_Overwrite [logs, "\n"]
    spinner = coloredSpinner defaultSpinnerTheme

-- | How nested spinner logs should be displayed
concatStack :: String -> [Text] -> String
concatStack mark = drop 1 . foldr flatten "" . zip (mark : repeat arrow) . fmap T.unpack
  where flatten (m, s) acc = unwords [acc, m, s]
        arrow = withColor Cyan "▶"

-- | A spinner is simply an infinite list of strings that supplant each other in a delayed loop, creating the
-- animation of a "spinner".
type Spinner = [String]

defaultSpinner :: SpinnerTheme -> Spinner
defaultSpinner = cycle

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = defaultSpinner . fmap (withColor Cyan)

-- | Run a spinner with a monadic function that defines how to represent the individual spinner characters.
runSpinner :: MonadIO m => Spinner -> (String -> m ()) -> m ()
runSpinner spinner f = forM_ spinner $ f >=> const delay
  where
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.

type SpinnerTheme = [String]

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

withColor :: Color -> String -> String
withColor color s = mconcat
  [ setSGRCode [SetColor Foreground Vivid color]
  , s
  , setSGRCode [Reset]
  ]

-- | Code for https://en.wikipedia.org/wiki/Enquiry_character
enquiryCode :: String
enquiryCode = "\ENQ"

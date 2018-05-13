{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Provides a simple CLI spinner that interoperates cleanly with the rest of the logging output.
module Obelisk.CLI.Spinner (withSpinner') where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (Color (Cyan, Green, Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), setSGRCode)

import Obelisk.CLI.Logging (LoggingConfig, Output (..), handleLog)

-- | Run an action with a CLI spinner.
withSpinner'
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => LoggingConfig -> Text -> m a -> m a
withSpinner' conf s = bracket' run cleanup . const
  where
    -- TODO: Can we obviate passing LoggingConfig just to use forkIO?
    run = liftIO $ forkIO $ flip runLoggingT (handleLog conf) $ do
      runSpinner (coloredSpinner defaultSpinnerTheme) $ \c -> do
        logMessage $ Output_Overwrite [c, " ", T.unpack s]
    cleanup failed tid = do
      liftIO $ killThread tid
      logMessage Output_ClearLine
      let mark = if failed
            then withColor Red "✖"
            else withColor Green "✔"
      logMessage $ Output_Overwrite [mark, " ", T.unpack s, "\n"]

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


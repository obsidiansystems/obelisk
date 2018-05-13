{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.CLI.Spinner (withSpinner') where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (Color (Green, Magenta, Red), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), setSGRCode)

import Obelisk.CLI.Logging (LoggingConfig, Output (..), handleLog)

-- | Run an action with a CLI spinner. You should use the more convenient
-- `Obelisk.Command.Utils.withSpinner` instead.
withSpinner'
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => LoggingConfig -> Text -> m a -> m a
withSpinner' conf s = bracket' run cleanup . const
  where
    -- TODO: Can we obviate passing LoggingConfig just to use forkIO?
    run = liftIO $ forkIO $ flip runLoggingT (handleLog conf) $ do
      forM_ spin $ \e -> do
        logMessage $ Output_Overwrite [e, " ", T.unpack s]
        delay
    cleanup failed tid = do
      liftIO $ killThread tid
      logMessage Output_ClearLine
      let mark = if failed then withColor Red "✖" else withColor Green "✔"
      logMessage $ Output_Overwrite [mark, " ", T.unpack s, "\n"]
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.
    spin = coloredSpinner spinnerCircleHalves

-- Spinner types

type Spinner = [String]

spinner :: SpinnerTheme -> Spinner
spinner = cycle

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = spinner . fmap (withColor Magenta)

withColor :: Color -> String -> String
withColor color s = mconcat
  [ setSGRCode [SetColor Foreground Vivid color]
  , s
  , setSGRCode [Reset]
  ]


-- Some spinners from https://github.com/sindresorhus/cli-spinners/blob/master/spinners.json

type SpinnerTheme = [String]

spinnerCircleHalves :: SpinnerTheme
spinnerCircleHalves = ["◐", "◓", "◑", "◒"]

-- | Like `bracket` but the `release` function can know whether an exception was raised
bracket' :: MonadMask m => m a -> (Bool -> a -> m b) -> (a -> m c) -> m c
bracket' acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release True resource
  _ <- release False resource
  return result

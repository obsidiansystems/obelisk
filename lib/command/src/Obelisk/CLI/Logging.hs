{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.CLI.Logging
  ( Severity (Error, Warning, Notice)
  , LoggingConfig (..)
  , Output (..)
  , failWith
  , putLog
  , handleLog
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Monad ((>=>))
import Control.Monad.Catch (MonadMask, bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Console.ANSI (Color (Red, Yellow), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), clearLine, setSGR)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (hFlush, stdout)

import Control.Monad.Log (MonadLog, Severity (..), WithSeverity (..), logMessage)


data LoggingConfig = LoggingConfig
  { _loggingConfig_level :: Severity  -- TODO: Start using this.
  , _loggingConfig_lock :: MVar Bool  -- Whether the last message was raw
  }

data Output
  = Output_Log (WithSeverity Text)  -- Regular logging message (with colors and newlines)
  | Output_Raw [String]  -- Raw text messages to print using `putStr`
  | Output_ClearLine  -- Clears the line
  deriving (Eq, Show, Ord)

isRaw :: Output -> Bool
isRaw = \case
  Output_Raw _ -> True
  _ -> False

handleLog :: MonadIO m => LoggingConfig -> Output -> m ()
handleLog c current = do
  let lock = _loggingConfig_lock c
  liftIO $ modifyMVar_ lock $ \raw -> do
    case (raw, current) of
      (True, Output_Log _) -> do
        -- If the last output was raw, clear the line prior to logging a non-raw
        -- message.
        handleLog' Output_ClearLine
        handleLog' current
      _ -> do
        handleLog' current
    return $ isRaw current

handleLog' :: MonadIO m => Output -> m ()
handleLog' = \case
  Output_Log m -> liftIO $ writeLog m
  Output_Raw xs -> liftIO $ putStrs xs
  Output_ClearLine -> liftIO $ do
    -- Go to the first column and clear the whole line
    putStrs ["\r"]
    clearLine
  where
    -- putStr xs, and flush at the end.
    putStrs :: [String] -> IO ()
    putStrs = mapM_ putStr >=> const (hFlush stdout)

-- | Safely log a message to the console. This is what we should use.
putLog :: MonadLog Output m => Severity -> Text -> m ()
putLog sev = logMessage . Output_Log . WithSeverity sev

-- | Like `putLog Error` but also abrupts the program.
failWith :: (MonadIO m, MonadLog Output m) => Text -> m a
failWith s = do
  putLog Alert s
  liftIO $ exitWith $ ExitFailure 2

-- | Write log to stdout with colors
-- TODO: Disable colours when not on interactive terminal
writeLog :: (MonadIO m, MonadMask m) => WithSeverity Text -> m ()
writeLog (WithSeverity severity s) = case sevColor severity of
  Just setColor -> bracket_ setColor reset $ liftIO (T.putStr s)
  Nothing -> liftIO $ T.putStrLn s
  where
    -- We must reset *before* the newline, else the cursor won't be reset.
    reset = liftIO $ setSGR [Reset] >> T.putStrLn ""
    sevColor sev = if
      | sev <= Error -> Just $ liftIO $ setSGR [SetColor Foreground Vivid Red]
      | sev <= Warning -> Just $ liftIO $ setSGR [SetColor Foreground Vivid Yellow]
      | otherwise -> Nothing

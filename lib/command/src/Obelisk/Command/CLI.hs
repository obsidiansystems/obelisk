{-# LANGUAGE LambdaCase #-}
module Obelisk.Command.CLI where

import Control.Exception (Exception, catch, handle, throw)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Typeable (Typeable)
import System.Exit (ExitCode (ExitFailure), exitWith)

import System.Console.ANSI
import System.Console.Questioner (dots1Spinner, stopIndicator)

newtype AppError = AppError Text
  deriving (Show, Typeable)

instance Exception AppError

withSpinner
  :: String  -- ^ Text to print alongside the spinner
  -> Maybe String  -- ^ Optional text to print at the end
  -> IO a  -- ^ Action to run and wait for
  -> IO a
withSpinner s e f = do
  spinner <- dots1Spinner (1000 * 200) s

  -- Upon exception (typically raised by putErrorAndExit), stop the spinner, log the error and exit.
  result <- catch f $ \exc -> do
    stopIndicator spinner
    case exc of
      AppError err -> putError err
    exitWith $ ExitFailure 2

  -- Upon no exceptions, stop the spinner and log the optional exit message.
  stopIndicator spinner
  case e of
    Just exitMsg -> putInfo $ T.pack exitMsg
    _ -> return ()
  return result

data Level = Level_Normal | Level_Warning | Level_Error

runCLI :: IO () -> IO ()
runCLI = handle $ \(AppError err) -> putError err

-- | Prefer this over the alternative ways to exit the program with an error message as it automatically
-- takes care of cleaning on spinners.
-- TODO: Replace the uses of 'fail' throughout Stela to use this function.
putErrorAndExit :: Text -> IO ()
putErrorAndExit = throw . AppError

putError :: Text -> IO ()
putError = putMsg Level_Error

putWarning :: Text -> IO ()
putWarning = putMsg Level_Warning

putInfo :: Text -> IO ()
putInfo = putMsg Level_Normal

putMsg :: Level -> Text -> IO ()
putMsg level s = do
  setColor level
  T.putStrLn s
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

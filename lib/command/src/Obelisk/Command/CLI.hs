{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module Obelisk.Command.CLI where

import Control.Exception (finally)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle.FD (stdout)
import System.Environment (getArgs)
import System.IO (hIsTerminalDevice)

import System.Console.ANSI
import System.Console.Questioner (dots1Spinner, stopIndicator)

-- TODO: This doesn't handle the put* line of functions below, in that: when we print anything while the
-- spinner is already running, it won't appear correctly in the terminal. The exception is `failWith` which
-- raises an exception (that gets handled properly here). One solution to fix this problem is to run a
-- singleton spinner thread and interact with it in order to print something.
withSpinner
  :: String  -- ^ Text to print alongside the spinner
  -> Maybe String  -- ^ Optional text to print at the end
  -> IO a  -- ^ Action to run and wait for
  -> IO a
withSpinner s e f = do
  isTerm <- hIsTerminalDevice stdout
  -- When running in bash completion, disable the spinner. TODO: Do this using ReaderT and config.
  inBashCompletion <- isInfixOf "bash-completion" . unwords <$> getArgs
  if | not isTerm || inBashCompletion -> f
     | otherwise -> do
      spinner <- dots1Spinner (1000 * 200) s
      result <- finally f $ do
        stopIndicator spinner
      case e of
        Just exitMsg -> putInfo $ T.pack exitMsg
        _ -> return ()
      return result

data Level = Level_Normal | Level_Warning | Level_Error

-- TODO: Handle this error cleanly when evaluating outside of `withSpinner` (eg: runCLI)
failWith :: Text -> IO a
failWith = ioError . userError . T.unpack

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

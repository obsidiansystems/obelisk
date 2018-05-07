module Obelisk.Command.CLI where

import Control.Exception (finally)

import System.Console.Questioner (dots1Spinner, stopIndicator)

withSpinner
  :: String  -- ^ Text to print alongside the spinner
  -> Maybe String  -- ^ Optional text to print at the end
  -> IO a  -- ^ Action to run and wait for
  -> IO a
withSpinner s e f = do
  spinner <- dots1Spinner (1000 * 200) s
  finally f $ do
    stopIndicator spinner
    case e of
      Just exitMsg -> putStrLn exitMsg
      Nothing -> return ()

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Package for writing great CLI apps.
module Obelisk.CLI
  ( withSpinner
  , cliDemo
  , module Obelisk.CLI.Logging
  , module Obelisk.CLI.Process
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (proc)

import Obelisk.CLI.Logging
import Obelisk.CLI.Process
import Obelisk.CLI.Spinner

import Obelisk.App (MonadObelisk, _obelisk_logging, _obelisk_noSpinner)

-- NOTE: Only these functions are hardcoded to Obelisk, but everything under `Obelisk.CLI` is independent of
-- Obelisk, and can be made a library of its own.

withSpinner :: MonadObelisk m => Text -> m a -> m a
withSpinner s action = do
  conf <- ask
  case _obelisk_noSpinner conf of
    True -> putLog Notice s >> action
    False -> withSpinner' (_obelisk_logging conf) s action

cliDemo :: MonadObelisk m => m ()
cliDemo = do
  putLog Notice "This demo will showcase the CLI library functionality"
  withSpinner "Running some long-running task" $ do
    liftIO $ threadDelay 1000000
    putLog Error "Some user error while spinning"
    liftIO $ threadDelay 1000000
    putLog Notice "This is some info mesage"
    putLog Warning "And now a warning as well"
    liftIO $ threadDelay 1000000
  putLog Notice "Now we start a 2nd spinner, run a couple of process, the last of which fails:"
  withSpinner "Looking around" $ do
    liftIO $ threadDelay 1000000
    output <- readProcessAndLogStderr Notice $ proc "ls" ["-l", "/"]
    putLog Notice $ "Output was: " <> T.pack output
    liftIO $ threadDelay 1000000
    callProcessAndLogOutput Notice $ proc "ls" ["-l", "/does-not-exist"]
    liftIO $ threadDelay 1000000
    failWith "Something dangerous happened"

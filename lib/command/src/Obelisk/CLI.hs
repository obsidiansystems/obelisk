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

-- NOTE: Everything under `Obelisk.CLI` is independent of the rest of Obelisk, with the exception of the
-- below functions, as they read the obelisk cli config using MonadReader.

withSpinner :: MonadObelisk m => Text -> m a -> m a
withSpinner s action = do
  conf <- ask
  case _obelisk_noSpinner conf of
    True -> putLog Notice s >> action
    False -> withSpinner' (_obelisk_logging conf) s action

cliDemo :: MonadObelisk m => m ()
cliDemo = withSpinner "CLI Demo" $ do
  putLog Notice "This demo will showcase the CLI library functionality"
  withSpinner "Running some long-running task" $ do
    delay
    withSpinner "Nested task" $ do
      delay
      putLog Notice "In nested task"
      withSpinner "Inbetween" $ do
        withSpinner "Nested task" $ do
          delay
          putLog Notice "This task has the same name and still works"
        delay
      delay
      putLog Error "Top nested task finished"
    delay
    putLog Error "Some user error while spinning"
    delay
    putLog Notice "This is some info mesage"
    putLog Warning "And now a warning as well"
    delay
  putLog Notice "Now we start a 2nd spinner, run a couple of process, the last of which fails:"
  withSpinner "Looking around" $ do
    delay
    output <- readProcessAndLogStderr Notice $ proc "ls" ["-l", "/"]
    putLog Notice $ "Output was: " <> T.pack output
    delay
    callProcessAndLogOutput (Notice, Error) $ proc "ls" ["-l", "/does-not-exist"]
    delay
    failWith "Something dangerous happened"
  where
    delay = liftIO $ threadDelay 1000000

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.CLI.Demo where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Process (proc)

import Control.Monad.Catch (MonadMask)

import Obelisk.CLI

cliDemo
  :: (MonadIO m, MonadMask m, Cli m, HasCliConfig m)
  => m ()
cliDemo = withSpinner "CLI Demo" $ do
  putLog Notice "This demo will showcase the CLI library functionality"
  withSpinner "A long-running task" $ do
    delay
    withSpinner "Nested task" $ do
      delay
      putLog Notice "In nested task"
      withSpinner "Inbetween" $ do
        withSpinner "Runnning something specific" $ do
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

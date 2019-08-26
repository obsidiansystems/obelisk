{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.CliApp.Demo where

import Control.Concurrent (threadDelay)
import Control.Monad.Fail
import Control.Monad.IO.Class
import Data.Semigroup ((<>))
import qualified Data.Text as T
import System.Process (proc)

import Control.Monad.Catch (MonadMask)

import Obelisk.CliApp

cliDemo
  :: ( MonadIO m, MonadMask m, MonadFail m
     , CliLog m, HasCliConfig e m, CliThrow e m, AsProcessFailure e, AsUnstructuredError e)
  => m ()
cliDemo = withSpinner "CLI Demo" $ do
  putLog Notice "This demo will showcase the CLI library functionality"
  _ <- withSpinner' "Searching long for something" (Just $ \c -> "Discovered " <> T.pack (show c) <> " objects") $ do
    delay
    withSpinner "Nested task" $ do
      delay
      putLog Notice "In nested task"
      -- This is a 'no trail' spinner that won't leave a trail (even its sub spinners)
      withSpinnerNoTrail "In between, doing something temporary" $ do
        withSpinner "Runnning something deep inside" $ do
          delay
          putLog Notice "You can still log from inside no-trail spinners"
        delay
      delay
      putLog Error "Top nested task finished"
    delay
    putLog Error "Some user error while spinning"
    delay
    putLog Notice "This is some info mesage"
    putLog Warning "And now a warning as well"
    delay
    return (42 :: Integer)
  putLog Notice "Now we start a 2nd spinner, run a couple of process, the last of which fails:"
  withSpinner "Looking around" $ do
    delay
    output <- readProcessAndLogStderr Notice $ proc "ls" ["-l", "/"]
    putLog Notice $ "Output was: " <> output
    delay
    callProcessAndLogOutput (Notice, Error) $ proc "ls" ["-l", "/does-not-exist"]
    delay
    failWith "Something dangerous happened"
  where
    delay = liftIO $ threadDelay 1000000

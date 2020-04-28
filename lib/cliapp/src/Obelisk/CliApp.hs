-- | Package for writing great CLI apps.
--
-- See Demo.hs for an example
--
-- This package should eventually be made its own library.
module Obelisk.CliApp
  (
  -- .Types
    CliLog
  , CliThrow
  , CliT(..)
  , runCli
  , CliConfig
  , HasCliConfig
  , getCliConfig
  , Output

  -- .Spinner
  , withSpinner
  , withSpinnerNoTrail
  , withSpinner'

  -- .Logging
  , AsUnstructuredError (..)
  , newCliConfig
  , getLogLevel
  , putLog
  , failWith
  , errorToWarning
  , withExitFailMessage

  -- Control.Monad.Log
  , Severity (..)

  -- .Process
  , AsProcessFailure (..)
  , ProcessFailure (..)
  , ProcessSpec (..)
  , callCommand
  , callProcess
  , callProcessAndLogOutput
  , createProcess_
  , overCreateProcess
  , proc
  , readCreateProcessWithExitCode
  , readProcessAndLogOutput
  , readProcessAndLogStderr
  , readProcessJSONAndLogStderr
  , reconstructCommand
  , setCwd
  , setDelegateCtlc
  , setEnvOverride
  , shell
  , waitForProcess
  ) where

import Control.Monad.Log (Severity (..))

import Obelisk.CliApp.Logging
import Obelisk.CliApp.Process
import Obelisk.CliApp.Spinner
import Obelisk.CliApp.Types

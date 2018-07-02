-- | Package for writing great CLI apps.
--
-- See Demo.hs for an example
--
-- This package should eventually be made its own library.
module Obelisk.CliApp
  (
  -- .Types
    Cli
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
  , newCliConfig
  , getLogLevel
  , putLog
  , failWith
  , withExitFailMessage

  -- Control.Monad.Log
  , Severity (..)

  -- .Process
  , readProcessAndLogStderr
  , callProcessAndLogOutput
  , createProcess_
  , callProcess
  , callCommand
  ) where

import Control.Monad.Log (Severity (..))

import Obelisk.CliApp.Logging
import Obelisk.CliApp.Process
import Obelisk.CliApp.Spinner
import Obelisk.CliApp.Types

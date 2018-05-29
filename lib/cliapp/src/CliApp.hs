-- | Package for writing great CLI apps.
--
-- See Demo.hs for an example
--
-- This package should eventually be made its own library.
module CliApp
  (
  -- .Types
    Cli
  , CliT(..)
  , runCli
  , CliConfig
  , HasCliConfig
  , getCliConfig

  -- .Spinner
  , withSpinner

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

import CliApp.Logging
import CliApp.Process
import CliApp.Spinner
import CliApp.Types

-- | Package for writing great CLI apps.
--
-- See Demo.hs for an example
--
-- This package should eventually be made its own library.
module Obelisk.CLI
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

import Obelisk.CLI.Logging
import Obelisk.CLI.Process
import Obelisk.CLI.Spinner
import Obelisk.CLI.Types

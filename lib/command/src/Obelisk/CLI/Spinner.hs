{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Obelisk.CLI.Spinner (withSpinner') where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Log (MonadLog, logMessage, runLoggingT)
import Control.Monad.Reader (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import System.Console.ANSI (Color (Magenta), ColorIntensity (Vivid), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor), setSGRCode)

import Obelisk.CLI.Logging (LoggingConfig, Output (..), handleLog)

-- | Run an action with a CLI spinner. You should use the more convenient
-- `Obelisk.Command.Utils.withSpinner` instead.
withSpinner'
  :: (MonadIO m, MonadMask m, MonadLog Output m)
  => LoggingConfig -> Text -> m a -> m a
withSpinner' c s = bracket run cleanup . const
  where
    -- TODO: Can we obviate passing LoggingConfig just to use forkIO?
    run = liftIO $ forkIO $ flip runLoggingT (handleLog c) $ do
      forM_ (coloredSpinner spinnerPoint) $ \e -> do
        logMessage $ Output_Raw ["\r", e, " ", T.unpack s]
        delay
    cleanup tid = do
      liftIO $ killThread tid
      logMessage Output_ClearLine
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.

type Spinner = [String]

spinner :: SpinnerTheme -> Spinner
spinner = cycle

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = spinner . fmap withColor
  where
    withColor s = mconcat
      [ setSGRCode [SetColor Foreground Vivid color]
      , s
      , setSGRCode [Reset]
      ]
    color = Magenta

-- Some spinners from https://github.com/sindresorhus/cli-spinners/blob/master/spinners.json

type SpinnerTheme = [String]

spinnerPoint :: SpinnerTheme
spinnerPoint =
  [ "∙∙∙"
  , "●∙∙"
  , "∙●∙"
  , "∙∙●"
  , "∙∙∙"
  ]

-- spinnerCircleHalves :: SpinnerTheme
-- spinnerCircleHalves = ["◐", "◓", "◑", "◒"]

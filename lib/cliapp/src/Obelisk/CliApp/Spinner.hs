{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Provides a simple CLI spinner that interoperates cleanly with the rest of the logging output.
module Obelisk.CliApp.Spinner
  ( withSpinner
  , withSpinnerNoTrail
  , withSpinner'
  ) where

import Control.Concurrent (killThread, threadDelay)
import Control.Monad (forM_, (>=>))
import Control.Monad.Catch (MonadMask, mask, onException)
import Control.Monad.IO.Class
import Control.Monad.Log (Severity (..), logMessage)
import Data.IORef
import qualified Data.List as L
import Data.Maybe (isNothing)
import Data.Text (Text)
import System.Console.ANSI (Color (Blue, Cyan, Green, Red))

import Obelisk.CliApp.Logging (allowUserToMakeLoggingVerbose, fork, putLog)
import Obelisk.CliApp.TerminalString (TerminalString (..), enquiryCode)
import Obelisk.CliApp.Theme
import Obelisk.CliApp.Types (CliLog, CliConfig (..), HasCliConfig, Output (..), getCliConfig)

-- | Run an action with a CLI spinner.
withSpinner
  :: (MonadIO m, MonadMask m, CliLog m, HasCliConfig e m)
  => Text -> m a -> m a
withSpinner s = withSpinner' s $ Just $ const s

-- | A spinner that leaves no trail after a successful run.
--
-- Use if you wish the spinner to be ephemerally visible to the user.
--
-- The 'no trail' property automatically carries over to sub-spinners (in that
-- they won't leave a trail either).
withSpinnerNoTrail
  :: (MonadIO m, MonadMask m, CliLog m, HasCliConfig e m)
  => Text -> m a -> m a
withSpinnerNoTrail s = withSpinner' s Nothing

-- | Advanced version that controls the display and content of the trail message.
withSpinner'
  :: (MonadIO m, MonadMask m, CliLog m, HasCliConfig e m)
  => Text
  -> Maybe (a -> Text) -- ^ Leave an optional trail with the given message creator
  -> m a
  -> m a
withSpinner' msg mkTrail action = do
  cliConf <- getCliConfig
  let noSpinner = _cliConfig_noSpinner cliConf
  if noSpinner
    then putLog Notice msg >> action
    else bracket' run cleanup $ const action
  where
    run = do
      -- Add this log to the spinner stack, and start a spinner if it is top-level.
      modifyStack pushSpinner >>= \case
        True -> do -- Top-level spinner; fork a thread to manage output of anything on the stack
          ctrleThread <- fork $ allowUserToMakeLoggingVerbose enquiryCode
          cliConf <- getCliConfig
          let theme = _cliConfig_theme cliConf
              spinner = coloredSpinner $ _cliTheme_spinner theme
          spinnerThread <- fork $ runSpinner spinner $ \c -> do
            logs <- renderSpinnerStack theme c . snd <$> readStack
            logMessage $ Output_Overwrite logs
          pure [ctrleThread, spinnerThread]
        False -> -- Sub-spinner; nothing to do.
          pure []
    cleanup tids resultM = do
      liftIO $ mapM_ killThread tids
      logMessage Output_ClearLine
      cliConf <- getCliConfig
      let theme = _cliConfig_theme cliConf
      logsM <- modifyStack $ (popSpinner theme) $ case resultM of
        Nothing ->
          ( TerminalString_Colorized Red $ _cliTheme_failed $ _cliConfig_theme cliConf
          , Just msg  -- Always display final message if there was an exception.
          )
        Just result ->
          ( TerminalString_Colorized Green $ _cliTheme_done $ _cliConfig_theme cliConf
          , mkTrail <*> pure result
          )
      -- Last message, finish off with newline.
      forM_ logsM $ logMessage . Output_Write
    pushSpinner (flag, old) =
      ( (isTemporary : flag, TerminalString_Normal msg : old)
      , null old -- Is empty?
      )
      where
        isTemporary = isNothing mkTrail
    popSpinner mTextEncoding (mark, trailMsgM) (flag, old) =
      ( (newFlag, new)
      -- With final trail spinner message to render
      , renderSpinnerStack mTextEncoding mark . (: new) . TerminalString_Normal <$> (
          if inTemporarySpinner then Nothing else trailMsgM
          )
      )
      where
        inTemporarySpinner = or newFlag  -- One of our parent spinners is temporary
        newFlag = drop 1 flag
        new = L.delete (TerminalString_Normal msg) old
    readStack = liftIO . readIORef
      =<< fmap _cliConfig_spinnerStack getCliConfig
    modifyStack f = liftIO . flip atomicModifyIORef' f
      =<< fmap _cliConfig_spinnerStack getCliConfig

-- | How nested spinner logs should be displayed
renderSpinnerStack
  :: CliTheme
  -> TerminalString  -- ^ That which comes before the final element in stack
  -> [TerminalString]  -- ^ Spinner elements in reverse order
  -> [TerminalString]
renderSpinnerStack theme mark = L.intersperse space . go . L.reverse
  where
    go [] = []
    go (x:[]) = mark : [x]
    go (x:xs) = arrow : x : go xs
    arrow = TerminalString_Colorized Blue $ _cliTheme_arrow theme
    space = TerminalString_Normal " "

-- | A spinner is simply an infinite list of strings that supplant each other in a delayed loop, creating the
-- animation of a "spinner".
type Spinner = [TerminalString]

coloredSpinner :: SpinnerTheme -> Spinner
coloredSpinner = cycle . fmap (TerminalString_Colorized Cyan)

-- | Run a spinner with a monadic function that defines how to represent the individual spinner characters.
runSpinner :: MonadIO m => Spinner -> (TerminalString -> m ()) -> m ()
runSpinner spinner f = forM_ spinner $ f >=> const delay
  where
    delay = liftIO $ threadDelay 100000  -- A shorter delay ensures that we update promptly.

-- | Like `bracket` but the `release` function can know whether an exception was raised
bracket' :: MonadMask m => m a -> (a -> Maybe c -> m b) -> (a -> m c) -> m c
bracket' acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release resource Nothing
  _ <- release resource $ Just result
  return result

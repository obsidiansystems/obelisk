module Obelisk.ExecutableConfig.Internal where

import Control.Exception
import System.IO.Error

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist f = catchJust doesNotExist (Just <$> f) $ const $ return Nothing
  where
    doesNotExist e = if isDoesNotExistError e then Just () else Nothing

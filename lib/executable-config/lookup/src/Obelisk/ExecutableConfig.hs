{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Data.Bool (bool)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import System.IO.Error

get :: Text -> IO (Maybe Text)
get path = do
  path' <- override path (path <> ".dev")
  catchJust doesNotExist (fmap Just $ T.readFile $ T.unpack path') (\_ -> pure Nothing)
  where
    -- Return p1 if it exists; otherwise return p.
    override p p1 = bool p p1 <$> doesFileExist (T.unpack p1)
    doesNotExist = \e -> if isDoesNotExistError e then Just () else Nothing

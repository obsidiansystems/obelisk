module Obelisk.ExecutableConfig (get) where

import Control.Exception
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.FilePath.Posix ((</>))
import System.IO.Error

get :: Text -> IO (Maybe Text)
get path = do
  let doesNotExist = \e -> if isDoesNotExistError e then Just () else Nothing
  catchJust doesNotExist (fmap Just $ T.readFile $ T.unpack path) (\_ -> pure Nothing)

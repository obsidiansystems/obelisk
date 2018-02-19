module Obelisk.ExecutableConfig (get) where

import Data.Text (Text)

get :: Text -> IO (Maybe Text)
get _ = return Nothing

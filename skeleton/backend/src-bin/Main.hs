{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Control.Lens
import Data.Maybe
import Data.Text.Encoding
import System.Environment
import qualified Text.URI as URI
import Text.URI.Lens

import Obelisk.ExecutableConfig (get)

import Backend

main :: IO ()
main = do
  Just route <- get "common/route"
  port <- case URI.mkURI route of
    Left err -> fail $ show err
    Right uri -> return $ fromMaybe 8000 $ uri ^? uriAuthority . _Right . authPort . _Just
  withArgs ["--port", show port] backend

{-# LANGUAGE OverloadedStrings #-}
import Control.Exception
import Data.Text.Encoding
import Obelisk.ExecutableConfig (get)
import System.Environment
import URI.ByteString

import Backend

main :: IO ()
main = do
  Just route <- get "common/route"
  port <- case parseURI strictURIParserOptions $ encodeUtf8 route of
    Left err -> fail $ show err
    Right uri -> return $ maybe 8000 portNumber $ authorityPort =<< uriAuthority uri
  withArgs ["--port", show port] backend


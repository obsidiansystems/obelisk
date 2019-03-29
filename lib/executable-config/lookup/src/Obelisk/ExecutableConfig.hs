module Obelisk.ExecutableConfig (get, getFrontendConfigs) where

import Control.Exception
import Data.List (sortOn)
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.Directory
import System.FilePath.Posix ((</>))
import System.IO.Error


get :: Text -> IO (Maybe Text)
get path = do
  let doesNotExist = \e -> if isDoesNotExistError e then Just () else Nothing
  catchJust doesNotExist (fmap Just $ T.readFile $ T.unpack path) (\_ -> pure Nothing)

getFrontendConfigs :: IO [(Text, Text)]
getFrontendConfigs = do
  cfgC <- getConfigs "config/common"
  cfgF <- getConfigs "config/frontend"
  return $ sortOn fst $ cfgC <> cfgF

getConfigs :: FilePath -> IO [(Text, Text)]
getConfigs fp = do
  dir <- doesDirectoryExist fp
  if dir
    then do
      ps <- listDirectory fp
      fmap Prelude.concat $ mapM (\p -> getConfigs $ fp </> p) ps
    else do
      file <- doesFileExist fp
      if file
        then do
          f <- T.readFile fp
          return [(T.pack fp, f)]
        else return []

module Obelisk.ExecutableConfig.Internal.ConfigDirectory where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.Directory
import System.FilePath.Posix ((</>))

getConfigsFromDirectory :: FilePath -> FilePath -> IO (Map Text Text)
getConfigsFromDirectory base fp = do
  dir <- doesDirectoryExist (base </> fp)
  if dir
    then do
      ps <- listDirectory (base </> fp)
      fmap mconcat $ mapM (\p -> getConfigsFromDirectory base $ fp </> p) ps
    else do
      file <- doesFileExist (base </> fp)
      if file
        then do
          f <- T.readFile (base </> fp)
          return $ Map.singleton (T.pack fp) f
        else return mempty

module Obelisk.ExecutableConfig (get, getFrontendConfigs) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.ByteString as BS
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Language.Javascript.JSaddle.WKWebView
import System.Directory
import System.FilePath.Posix ((</>))
import System.IO.Error

get :: Text -> IO (Maybe Text)
get name = fmap join $ mainBundleResourcePath >>= \mp -> forM mp $ \p -> 
  catchDoesNotExist  $ T.readFile $ T.unpack (T.decodeUtf8 p) </> T.unpack name

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist f = catchJust doesNotExist (Just <$> f) $ const $ return Nothing
  where
    doesNotExist e = if isDoesNotExistError e then Just () else Nothing


getFrontendConfigs :: IO [(Text, Text)]
getFrontendConfigs = fmap Prelude.concat $ mainBundleResourcePath >>= \mp -> forM mp $ \p -> do
  let base = T.unpack (T.decodeUtf8 p)
  cfgC <- getConfigs base "config/common"
  cfgF <- getConfigs base "config/frontend"
  return $ sortOn fst $ cfgC <> cfgF

getConfigs :: FilePath -> FilePath -> IO [(Text, Text)]
getConfigs base fp = do
  dir <- doesDirectoryExist (base </> fp)
  if dir
    then do
      ps <- listDirectory (base </> fp)
      fmap Prelude.concat $ mapM (\p -> getConfigs base $ fp </> p) ps
    else do
      file <- doesFileExist (base </> fp)
      if file
        then do
          f <- T.readFile (base </> fp)
          return [(T.pack fp, f)]
        else return []

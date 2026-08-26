module Obelisk.Configs.Internal.Directory where

import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.Directory
import System.FilePath.Posix

getConfigsFromDirectory :: FilePath -> IO (Map Text ByteString)
getConfigsFromDirectory base =
  doesDirectoryExist base >>= \case
    True -> do
      ps <- listDirectory base
      fmap mconcat $ forM ps $ \p -> do
        subdirConfigs <- getConfigsFromDirectory $ base </> p
        pure $ Map.mapKeys (T.pack . (p </>) . T.unpack) subdirConfigs
    False ->
      doesFileExist base >>= \case
        True -> Map.singleton "" <$> BS.readFile base
        False -> pure mempty

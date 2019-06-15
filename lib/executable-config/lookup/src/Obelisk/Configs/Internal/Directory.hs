{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Configs.Internal.Directory where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath.Posix

getConfigsFromDirectory :: FilePath -> IO (Map Text ByteString)
getConfigsFromDirectory base = doesDirectoryExist base >>= \case
  True -> do
    ps <- listDirectory base
    fmap mconcat $ forM ps $ \p -> do
      subdirConfigs <- getConfigsFromDirectory $ base </> p
      pure $ Map.mapKeys (T.pack . (p </>) . T.unpack) subdirConfigs
  False -> doesFileExist base >>= \case
    True -> Map.singleton "" <$> BS.readFile base
    False -> pure mempty

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Lookup where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import Control.Monad

getConfigs :: IO (Map Text ByteString)
getConfigs = getConfigsFromDirectory "config"
  where
    getConfigsFromDirectory :: FilePath -> IO (Map Text ByteString)
    getConfigsFromDirectory base = do
      doesDirectoryExist base >>= \case
        True -> do
          ps <- listDirectory base
          fmap mconcat $ forM ps $ \p -> do
            subdirConfigs <- getConfigsFromDirectory $ base </> p
            pure $ Map.mapKeys (T.pack . (p </>) . T.unpack) subdirConfigs
        False -> do
          doesFileExist base >>= \case
            True -> do
              f <- BS.readFile base
              pure $ Map.singleton "" f
            False -> do
              pure mempty

{-# LANGUAGE LambdaCase #-}
module Obelisk.Configs.Internal.Directory where

import Data.ByteString (ByteString)
import Data.Text (Text)

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

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Internal.ConfigDirectory where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Text as T
import Data.Text.IO as T
import System.Directory
import System.FilePath.Posix ((</>))

getConfigsFromDirectory :: FilePath -> IO (Map Text Text)
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
          f <- T.readFile base
          pure $ Map.singleton "" f
        False -> do
          pure mempty

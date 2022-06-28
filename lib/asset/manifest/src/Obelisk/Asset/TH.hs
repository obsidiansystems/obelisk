{-|
Description:
  Template Haskell for generating asset paths.
-}
module Obelisk.Asset.TH
  ( assetPath
  , staticAssetRaw
  , staticAssetHashed
  , staticAssetFilePath
  , staticAssetFilePathRaw
  ) where

import Obelisk.Asset.Gather

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath.Posix

-- | Produces the hashed path of a file
hashedAssetFilePath :: FilePath -> FilePath -> Q FilePath
hashedAssetFilePath root relativePath = do
  qAddDependentFile $ root </> relativePath
  runIO (toHashedPath root relativePath)

-- | Produces a string literal with the hashed path of the file
assetPath :: FilePath -> FilePath -> Q Exp
assetPath root relativePath =
  LitE . StringL <$> hashedAssetFilePath root relativePath

staticPrefix :: FilePath
staticPrefix = "/static"

staticAssetRaw :: FilePath -> Q Exp
staticAssetRaw fp = do
  exists <- runIO $ doesFileExist $ "static.out/" <> fp
  when (not exists) $
    fail $ "The file " <> fp <> " was not found in static.out"
  returnQ $ LitE $ StringL $ staticPrefix </> fp

staticAssetHashed :: FilePath -> FilePath -> Q Exp
staticAssetHashed root fp = do
  LitE . StringL . (staticPrefix </>) <$> hashedAssetFilePath root fp

staticAssetFilePathRaw :: FilePath -> FilePath -> Q Exp
staticAssetFilePathRaw root fp = returnQ $ LitE $ StringL $ root </> fp

staticAssetFilePath :: FilePath -> FilePath -> Q Exp
staticAssetFilePath root fp = do
  LitE . StringL . (root </>) <$> hashedAssetFilePath root fp

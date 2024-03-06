{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Functions for collecting up files to be processed by the asset pipeline
module Obelisk.Asset.Gather
  ( gatherHashedPaths
  , toHashedPath
  ) where

import Control.DeepSeq (force)
import Control.Monad (forM)
import qualified Data.ByteString.Lazy as LBS (readFile, toStrict)
import Data.Digest.Pure.SHA (bytestringDigest, sha256)
import Data.Map (Map)
import qualified Data.Map as Map (singleton)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import System.FilePath.Posix ((</>), splitFileName, normalise)
import System.Directory (listDirectory, doesFileExist)
import qualified Data.ByteString.Base16 as Base16

-- | Given a path, recursively explore it, creating hashed paths for all files found
gatherHashedPaths
  :: FilePath -- ^ The root of the directory to process
  -> IO (Map FilePath FilePath) -- ^ A mapping of original paths to hashed paths (all relative to the root)
gatherHashedPaths root = go ""
  where
    go :: FilePath -> IO (Map FilePath FilePath)
    go subdir = do
      subs <- listDirectory $ root </> subdir
      fmap mconcat $ forM subs $ \sub -> do
        let relativePath = subdir </> sub
        isFile <- doesFileExist $ root </> relativePath
        if isFile
          then do !hashedRelativePath <- force <$> toHashedPath root relativePath
                  return $ Map.singleton relativePath hashedRelativePath
          else go relativePath

-- | Given a root path and path relative to it, construct a hashed version of the relative path
toHashedPath
  :: FilePath -- ^ Root path
  -> FilePath -- ^ Relative path within root
  -> IO FilePath
toHashedPath root relativePath = do
  let path = root </> relativePath
  contents <- LBS.readFile path
  let hashPrefix = T.unpack $ decodeUtf8 $ Base16.encode $ LBS.toStrict $ bytestringDigest $ sha256 contents
      (dir, filename) = splitFileName relativePath
      !hashedRelativePath = force $ normalise $ dir </> (hashPrefix <> "-" <> filename)
  return hashedRelativePath

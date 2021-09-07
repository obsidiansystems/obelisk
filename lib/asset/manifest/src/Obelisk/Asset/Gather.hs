{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
-- | Functions for collecting up files to be processed by the asset pipeline
module Obelisk.Asset.Gather
  ( gatherHashedPaths
  , toHashedPath
  ) where

import Control.DeepSeq (force)
import Control.Monad (forM)
import Data.Bits (shift, (.|.), (.&.))
import qualified Data.ByteString.Builder as LBS (toLazyByteString, word8)
import qualified Data.ByteString.Lazy as LBS (readFile, toStrict, ByteString, length, index)
import Data.Char (ord)
import Data.Digest.Pure.SHA (bytestringDigest, sha256)
import Data.Map (Map)
import qualified Data.Map as Map (singleton)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector.Unboxed as UV ((!), fromList)
import Data.Word (Word8)
import System.FilePath.Posix ((</>), splitFileName, normalise)
import System.Directory (listDirectory, doesFileExist)

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
  let hashPrefix = T.unpack $ decodeUtf8 $ LBS.toStrict $ toNixBase32 $ bytestringDigest $ sha256 contents
      (dir, filename) = splitFileName relativePath
      !hashedRelativePath = force $ normalise $ dir </> (hashPrefix <> "-" <> filename)
  return hashedRelativePath

-- | Convert a ByteString to base 32 in the way that Nix does
toNixBase32 :: LBS.ByteString -> LBS.ByteString
toNixBase32 x = LBS.toLazyByteString $ mconcat $ map (LBS.word8 . (symbols UV.!) . fromIntegral) vals
  where vals = byteStringToQuintets x
        symbols = UV.fromList $ map (fromIntegral . ord) $ filter (`notElem` ("eotu" :: String)) $ ['0'..'9'] <> ['a'..'z']
        -- See https://github.com/NixOS/nix/blob/6f1743b1a5116ca57a60b481ee4083c891b7a334/src/libutil/hash.cc#L109
        byteStringToQuintets :: LBS.ByteString -> [Word8]
        byteStringToQuintets hash = map f [len-1, len-2 .. 0]
          where hashSize = fromIntegral $ LBS.length hash
                len = (hashSize * 8 - 1) `div` 5 + 1
                f n = let b = n * 5
                          (i, j) = b `divMod` 8
                          j' = fromIntegral j
                          --TODO: This is probably pretty slow; replace with something that doesn't use LBS.index
                          c = ((hash `LBS.index` i) `shift` (-j')) .|. (if i >= hashSize - 1 then 0 else (hash `LBS.index` (i + 1)) `shift` (8 - j'))
                      in c .&. 0x1f

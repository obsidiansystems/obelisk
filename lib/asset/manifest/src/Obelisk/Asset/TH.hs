{-|
Description:
  Template Haskell for generating asset paths.
-}

{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE CPP #-}

module Obelisk.Asset.TH
  ( assetPath
  , staticAssetRaw
  , staticAssetHashed
  , staticAssetFilePath
  , staticAssetFilePathRaw
  , staticAssetFileContent
  ) where

import Obelisk.Asset.Gather

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS

#if MIN_VERSION_template_haskell(2, 16, 0)
import qualified Data.ByteString.Internal as BSI
#endif

import qualified Data.ByteString.Char8 as Char8
import Data.ByteString.Unsafe (unsafePackAddressLen)

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

-- | Location of the symbolic link to static assets and resources.
staticOutPath :: FilePath
staticOutPath = "static.out"

-- | Embed a filepath via template haskell. Resources embedded this way
-- are requested from the "/static" route.
--
-- If the filepath can not be found in the static output directory,
-- this will throw a compile-time error.
staticAssetRaw :: FilePath -> Q Exp
staticAssetRaw = staticAssetWorker staticPrefix staticOutPath

staticAssetHashed :: FilePath -> FilePath -> Q Exp
staticAssetHashed root fp = do
  LitE . StringL . (staticPrefix </>) <$> hashedAssetFilePath root fp

-- | Embed a filepath via template haskell. Differently to 'staticAssetRaw'
-- this points to a local filepath instead of an URL during deployment.
--
-- If the filepath can not be found in the static output directory,
-- this will throw a compile-time error.
staticAssetFilePathRaw
  :: FilePath
  -- ^ Add this prefix directory to the embedded filepath @fp@.
  -> FilePath
  -- ^ Filepath you want to embed.
  -> Q Exp
staticAssetFilePathRaw root = staticAssetWorker root staticOutPath

staticAssetFilePath :: FilePath -> FilePath -> Q Exp
staticAssetFilePath root relativePath = do
  let fullPath = root </> relativePath
  qAddDependentFile fullPath
  pure $ LitE $ StringL fullPath

-- | @'staticAssetWorker' root staticOut fp@.
--
-- Produces @root </> fp@, but checks before that @fp@ has been copied
-- to 'staticOutPath' and produces a compilation error otherwise.
-- This helps finding typos in filepaths, etc... at compile-time instead of
-- run-time.
staticAssetWorker
  :: FilePath
  -- ^ Add this prefix directory to the embedded filepath @fp@.
  -> FilePath
  -- ^ Directory to which the filepath must have been copied.
  -- If @fp@ does not exist within this directory, this function will fail.
  -> FilePath
  -- ^ Filepath you want to embed.
  -> Q Exp
staticAssetWorker root staticOut fp = do
  exists <- runIO $ doesFileExist $ staticOut </> fp
  when (not exists) $
    fail $ "The file " <> fp <> " was not found in " <> staticOut
  returnQ $ LitE $ StringL $ root </> fp

-- | read  the file contents of a static asset at compile time into 'ByteString'
-- like 'embedFile' from package @file-embed@
--
-- > import qualified Data.ByteString
-- >
-- > myFile :: Data.ByteString.ByteString
-- > myFile = $(staticAssetFileContentRaw "dirName/fileName")
staticAssetFileContent :: FilePath -> FilePath -> Q Exp
staticAssetFileContent root fp = do
  qAddDependentFile $ root </> fp
  bs <- runIO (BS.readFile $ root </> fp)
  -- the following is copy-paste from
  -- https://hackage.haskell.org/package/file-embed-0.0.15.0/docs/src/Data.FileEmbed.html#bsToExp
  -- assuming template-haskell >= 2.8.0
  returnQ $ VarE 'unsafePerformIO
      `AppE` (VarE 'unsafePackAddressLen
      `AppE` LitE (IntegerL $ fromIntegral $ Char8.length bs)
#if MIN_VERSION_template_haskell(2, 16, 0)
      `AppE` LitE (bytesPrimL (
                let BSI.PS ptr off sz = bs
                in  mkBytes ptr (fromIntegral off) (fromIntegral sz))))
#else
      `AppE` LitE (StringPrimL $ BS.unpack bs))
#endif

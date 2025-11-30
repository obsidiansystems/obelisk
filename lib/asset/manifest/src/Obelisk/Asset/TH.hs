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

-- | Location of the symbolic link to static assets and resources.
staticOutPath :: FilePath
staticOutPath = "static.out"

-- | Embed a filepath via template haskell. Resources embedded this way
-- are requested from the "/static" route.
--
-- If the filepath can not be found in the static output directory,
-- this will throw a compile-time error.
staticAssetRaw :: FilePath -> FilePath -> Q Exp
staticAssetRaw staticName fp = staticAssetWorker staticPrefix staticOutPath staticName fp 

-- | Generate URL to reference handler which will serve this asset path
-- | where "/static" route maps to the base directory static.assets 
staticAssetHashed :: FilePath -> FilePath -> FilePath -> Q Exp
staticAssetHashed root staticPkgName fp = do
  LitE . StringL . (\p -> staticPrefix </> staticPkgName </> p) <$> hashedAssetFilePath root fp

-- | Embed a filepath via template haskell. Differently to 'staticAssetRaw'
-- this points to a local filepath instead of an URL during deployment.
--
-- If the filepath can not be found in the static output directory,
-- this will throw a compile-time error.
staticAssetFilePathRaw
  :: FilePath
  -- ^ Add this prefix directory to the embedded filepath @fp@.
  -> FilePath
  -- ^ Static Project specific prefix to avoid collisions
  -> FilePath
  -- ^ Filepath you want to embed.
  -> Q Exp
staticAssetFilePathRaw root staticName = staticAssetWorker root staticOutPath staticName

-- | Return the real location of 'relativePath' 
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
  -- ^ Base Directory to which the filepath must have been copied.
  -- If @fp@ does not exist within this directory, this function will fail.
  -> FilePath
  -- ^ Static Project Directory to which the filepath must have been copied.
  -- If @fp@ does not exist within this directory, this function will fail.
  -> FilePath
  -- ^ Filepath you want to embed.
  -> Q Exp
staticAssetWorker root staticOut staticName fp = do
  exists <- runIO $ doesFileExist $ staticOut </> staticName </> fp
  when (not exists) $
    fail $ "The file " <> fp <> " was not found in " <> staticOut </> staticName
  return $ LitE $ StringL $ root </> staticName </> fp

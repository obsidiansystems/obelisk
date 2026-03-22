{-# LANGUAGE BangPatterns #-}
-- | Static manifest Setup.hs hook. Runs @static\/generate@ to produce hashed
-- static assets, then generates the @Obelisk.Generated.Static.Instances@
-- module with @StaticFile@ instances for cache-busting paths.
module Obelisk.Setup.Manifest (main) where

import Distribution.Simple
import Control.DeepSeq (force)
import Control.Monad (forM)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , listDirectory
  )
import System.FilePath (splitFileName, normalise, (</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcess, waitForProcess, withCreateProcess)
import System.Exit (ExitCode (..))

import qualified Data.Map as Map

import Obelisk.Setup.Utils (findProjectRoot)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
      generateStaticModule
      preBuild simpleUserHooks args flags
  }

generateStaticModule :: IO ()
generateStaticModule = do
  projectRoot <- findProjectRoot

  let dataDir = projectRoot </> "static" </> "manifest" </> "data"
  createDirectoryIfMissing True dataDir

  hPutStrLn stderr "[Setup] Building static assets..."
  withCreateProcess ((proc (projectRoot </> "static" </> "generate") [dataDir </> "static"]) { cwd = Just projectRoot }) $
    \_ _ _ ph -> do
      ec <- waitForProcess ph
      case ec of
        ExitSuccess -> pure ()
        ExitFailure _ -> fail "[Setup] static/generate failed"

  hPutStrLn stderr "[Setup] Gathering hashed paths..."
  paths <- gatherHashedPaths (dataDir </> "static")

  let content = renderInstances paths
      moduleDir = projectRoot </> "static" </> "manifest" </> "src" </> "Obelisk" </> "Generated" </> "Static"

  createDirectoryIfMissing True moduleDir
  writeFile (moduleDir </> "Instances.hs") content
  hPutStrLn stderr $ "[Setup] Generated Obelisk.Generated.Static.Instances (" <> show (Map.size paths) <> " files)"

gatherHashedPaths :: FilePath -> IO (Map.Map FilePath FilePath)
gatherHashedPaths root = go ""
  where
    go subdir = do
      subs <- listDirectory (root </> subdir)
      fmap mconcat $ forM subs $ \sub -> do
        let relativePath = subdir </> sub
        isFile <- doesFileExist (root </> relativePath)
        if isFile
          then do !hashedPath <- force <$> toHashedPath root relativePath
                  pure $ Map.singleton relativePath hashedPath
          else go relativePath

toHashedPath :: FilePath -> FilePath -> IO FilePath
toHashedPath root relativePath = do
  output <- readCreateProcess (proc "sha256sum" [root </> relativePath]) ""
  let hash = takeWhile (/= ' ') output
      (dir, filename) = splitFileName relativePath
  pure $! normalise $ dir </> (hash <> "-" <> filename)

renderInstances :: Map.Map FilePath FilePath -> String
renderInstances paths = unlines $
  [ "{-# LANGUAGE DataKinds #-}"
  , "{-# LANGUAGE FlexibleInstances #-}"
  , "{-# LANGUAGE OverloadedStrings #-}"
  , "{-# OPTIONS_GHC -Wno-orphans #-}"
  , "module Obelisk.Generated.Static.Instances () where"
  , ""
  , "import Obelisk.Generated.Static.Class (StaticFile (..))"
  ] <> concatMap mkInstance (Map.toList paths)
  where
    mkInstance (original, hashed) =
      [ ""
      , "instance StaticFile " <> show original <> " where"
      , "  staticPath = " <> show hashed
      ]

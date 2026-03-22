-- | Static manifest Setup.hs hook. Runs @static\/generate@ to produce hashed
-- static assets, then generates the @Obelisk.Generated.Static@ module using
-- @obelisk-asset-manifest@.
module Obelisk.Setup.Static (main) where

import Distribution.Simple
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, waitForProcess, withCreateProcess)
import System.Exit (ExitCode (..))

import qualified Data.Map as Map

import Obelisk.Asset.Gather (gatherHashedPaths)
import Obelisk.Asset.Promoted (writeStaticModule)
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

  let dataDir = projectRoot </> "static" </> "generated" </> "data"
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

  let target = projectRoot </> "static" </> "generated"
  writeStaticModule paths target (T.pack "Obelisk.Generated.Static")
  hPutStrLn stderr $ "[Setup] Generated Obelisk.Generated.Static (" <> show (Map.size paths) <> " files)"

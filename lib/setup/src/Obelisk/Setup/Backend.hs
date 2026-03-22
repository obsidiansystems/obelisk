-- | Backend Setup.hs hook. Symlinks frontend assets into @backend\/data\/@
-- before build so the backend can serve them at runtime.
module Obelisk.Setup.Backend (main) where

import Distribution.Simple
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import Obelisk.Setup.Utils (findProjectRoot, symlink)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preBuild = \args flags -> do
      linkAssets
      preBuild simpleUserHooks args flags
  }

linkAssets :: IO ()
linkAssets = do
  projectRoot <- findProjectRoot
  let dataDir = projectRoot </> "backend" </> "data"
      frontendData = projectRoot </> "frontend" </> "data"

  createDirectoryIfMissing True dataDir

  symlink (frontendData </> "frontend.jsexe") (dataDir </> "frontend.jsexe")
  symlink (frontendData </> "static") (dataDir </> "static")

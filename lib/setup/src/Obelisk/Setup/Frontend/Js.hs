-- | Frontend Setup.hs hook. Cross-compiles the frontend with GHCJS in a
-- background thread during pre-build, then symlinks the output and static
-- assets into @frontend\/data\/@ after build completes.
module Obelisk.Setup.Frontend.Js (main) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (withOptimization)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , findExecutable
  )
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcess, waitForProcess, withCreateProcess)
import System.Exit (ExitCode (..), exitFailure)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (unless)

import Obelisk.Setup.Utils (crossCabalArgs, findProjectRoot, optLevelFlags, strip, symlink)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi hooks flags -> do
      envArgs <- crossCabalArgs
      let extraFlags = optLevelFlags (withOptimization lbi) <> envArgs
      resultVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
      _ <- forkIO $ do
        result <- try (buildFrontendWithGhcjs extraFlags)
        putMVar resultVar result
      buildHook simpleUserHooks pd lbi hooks flags
      hPutStrLn stderr "[Setup] Waiting for GHCJS frontend build..."
      result <- takeMVar resultVar
      case result of
        Left e -> do
          hPutStrLn stderr $ "[Setup] GHCJS build failed: " <> show e
          exitFailure
        Right () -> do
          hPutStrLn stderr "[Setup] GHCJS frontend build complete."
          linkFrontendAssets
  }

findGhcjsCabal :: IO (String, [String] -> [String])
findGhcjsCabal = do
  wrapper <- findExecutable "javascript-unknown-ghcjs"
  case wrapper of
    Just _ -> pure ("javascript-unknown-ghcjs", ("cabal" :))
    Nothing -> do
      direct <- findExecutable "javascript-unknown-ghcjs-cabal"
      case direct of
        Just _ -> pure ("javascript-unknown-ghcjs-cabal", id)
        Nothing -> fail "[Setup] Neither javascript-unknown-ghcjs nor javascript-unknown-ghcjs-cabal found on PATH. Are you in a nix shell?"

-- | Build the frontend executable with GHCJS.
-- Extra flags (e.g. @--ghc-options=-O0@) are forwarded to the cross cabal.
buildFrontendWithGhcjs :: [String] -> IO ()
buildFrontendWithGhcjs extraFlags = do
  projectRoot <- findProjectRoot
  (ghcjsCabal, mkArgs) <- findGhcjsCabal
  let distJs = projectRoot </> "dist-js"

  let inRoot cmd args = (proc cmd args) { cwd = Just projectRoot }
      callInRoot cmd args =
        withCreateProcess (inRoot cmd args) $ \_ _ _ ph -> do
          ec <- waitForProcess ph
          case ec of
            ExitSuccess -> pure ()
            ExitFailure _ -> fail "[Setup] GHCJS cabal build failed"

  hPutStrLn stderr "[Setup] Building frontend with GHCJS..."
  callInRoot ghcjsCabal (mkArgs (["build", "exe:frontend", "--builddir=" <> distJs] <> extraFlags))

linkFrontendAssets :: IO ()
linkFrontendAssets = do
  projectRoot <- findProjectRoot
  (ghcjsCabal, mkArgs) <- findGhcjsCabal
  let dataDir = projectRoot </> "frontend" </> "data"
      distJs = projectRoot </> "dist-js"

  createDirectoryIfMissing True dataDir

  let inRoot cmd args = (proc cmd args) { cwd = Just projectRoot }

  binPathRaw <- readCreateProcess (inRoot ghcjsCabal (mkArgs ["list-bin", "frontend", "--builddir=" <> distJs])) ""
  let binPath = strip binPathRaw
      jsexeDir = binPath <> ".jsexe"

  jsexeExists <- doesDirectoryExist jsexeDir
  unless jsexeExists $
    fail $ "[Setup] GHCJS output directory not found: " <> jsexeDir

  symlink jsexeDir (dataDir </> "frontend.jsexe")
  symlink (projectRoot </> "static" </> "generated" </> "data" </> "static") (dataDir </> "static")


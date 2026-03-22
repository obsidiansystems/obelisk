-- | Frontend Setup.hs hook for WASM. Cross-compiles the frontend with
-- wasm32-wasi in a background thread during pre-build, assembles the jsexe
-- directory (post-link.mjs, shim, wasi-shim), then symlinks the output and
-- static assets into @frontend\/data\/@ after build completes.
module Obelisk.Setup.Frontend.Wasm (main) where

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (withOptimization)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , findExecutable
  , listDirectory
  , removeFile
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), proc, readCreateProcess, waitForProcess, withCreateProcess)
import System.Exit (ExitCode (..), exitFailure)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Exception (SomeException, try)
import Control.Monad (forM_, unless)

import Obelisk.Setup.Utils (crossCabalArgs, findProjectRoot, optLevelFlags, strip, symlink)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pd lbi hooks flags -> do
      envArgs <- crossCabalArgs
      let extraFlags = optLevelFlags (withOptimization lbi) <> envArgs
      resultVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
      _ <- forkIO $ do
        result <- try (buildFrontendWithWasm extraFlags)
        putMVar resultVar result
      buildHook simpleUserHooks pd lbi hooks flags
      hPutStrLn stderr "[Setup] Waiting for WASM frontend build..."
      result <- takeMVar resultVar
      case result of
        Left e -> do
          hPutStrLn stderr $ "[Setup] WASM build failed: " <> show e
          exitFailure
        Right () -> do
          hPutStrLn stderr "[Setup] WASM frontend build complete."
          assembleAndLinkFrontend
  }

-- | Find the wasm32 cross cabal: either the @wasm32-unknown-wasi@ wrapper
-- (which takes @cabal@ as first arg) or the direct @wasm32-unknown-wasi-cabal@.
findWasmCabal :: IO (String, [String] -> [String])
findWasmCabal = do
  wrapper <- findExecutable "wasm32-unknown-wasi"
  case wrapper of
    Just _ -> pure ("wasm32-unknown-wasi", ("cabal" :))
    Nothing -> do
      direct <- findExecutable "wasm32-unknown-wasi-cabal"
      case direct of
        Just _ -> pure ("wasm32-unknown-wasi-cabal", id)
        Nothing -> fail "[Setup] Neither wasm32-unknown-wasi nor wasm32-unknown-wasi-cabal found on PATH. Are you in a nix shell?"

-- | Get the GHC libdir for the wasm32 cross-compiler (contains post-link.mjs).
getWasmGhcLibdir :: IO FilePath
getWasmGhcLibdir = do
  wrapper <- findExecutable "wasm32-unknown-wasi"
  raw <- case wrapper of
    Just _ -> readCreateProcess (proc "wasm32-unknown-wasi" ["ghc", "--print-libdir"]) ""
    Nothing -> do
      direct <- findExecutable "wasm32-unknown-wasi-ghc"
      case direct of
        Just _ -> readCreateProcess (proc "wasm32-unknown-wasi-ghc" ["--print-libdir"]) ""
        Nothing -> fail "[Setup] wasm32-unknown-wasi-ghc not found on PATH."
  pure (strip raw)

-- | Build the frontend executable with the wasm32 cross-compiler.
-- Extra flags (e.g. @--ghc-options=-O0@) are forwarded to the cross cabal.
buildFrontendWithWasm :: [String] -> IO ()
buildFrontendWithWasm extraFlags = do
  projectRoot <- findProjectRoot
  (wasmCabal, mkArgs) <- findWasmCabal
  let distWasm = projectRoot </> "dist-wasm"
      inRoot cmd args = (proc cmd args) { cwd = Just projectRoot }
      callInRoot cmd args =
        withCreateProcess (inRoot cmd args) $ \_ _ _ ph -> do
          ec <- waitForProcess ph
          case ec of
            ExitSuccess -> pure ()
            ExitFailure _ -> fail "[Setup] WASM cabal build failed"
  hPutStrLn stderr "[Setup] Building frontend with WASM..."
  callInRoot wasmCabal (mkArgs (["build", "exe:frontend", "--builddir=" <> distWasm] <> extraFlags))

-- | Assemble the jsexe directory and symlink it into @frontend\/data\/@.
assembleAndLinkFrontend :: IO ()
assembleAndLinkFrontend = do
  projectRoot <- findProjectRoot
  (wasmCabal, mkArgs) <- findWasmCabal
  let distWasm = projectRoot </> "dist-wasm"
      jsexeDir = distWasm </> "frontend.jsexe"
      dataDir = projectRoot </> "frontend" </> "data"
      inRoot cmd args = (proc cmd args) { cwd = Just projectRoot }

  -- Find the compiled .wasm binary
  binPathRaw <- readCreateProcess
    (inRoot wasmCabal (mkArgs ["list-bin", "frontend", "--builddir=" <> distWasm])) ""
  let wasmBin = strip binPathRaw

  -- Create jsexe assembly directory
  createDirectoryIfMissing True jsexeDir

  -- Copy frontend.wasm
  copyFile wasmBin (jsexeDir </> "frontend.wasm")

  -- Extract JSFFI bindings via post-link.mjs
  libdir <- getWasmGhcLibdir
  let postLinkMjs = libdir </> "post-link.mjs"
  hPutStrLn stderr $ "[Setup] Extracting JSFFI: " <> postLinkMjs
  withCreateProcess (proc "node" [postLinkMjs, "-i", wasmBin, "-o", jsexeDir </> "ghc_wasm_jsffi.js"]) $ \_ _ _ ph -> do
    ec <- waitForProcess ph
    case ec of
      ExitSuccess -> pure ()
      ExitFailure _ -> fail "[Setup] post-link.mjs failed"

  -- Write all.js shim
  writeFile (jsexeDir </> "all.js") shimJs

  -- Copy wasi-shim dist files
  copyWasiShim jsexeDir

  -- Optionally optimize
  optimizeWasm (jsexeDir </> "frontend.wasm")

  -- Symlink into frontend/data/
  createDirectoryIfMissing True dataDir
  symlink jsexeDir (dataDir </> "frontend.jsexe")
  symlink (projectRoot </> "static" </> "generated" </> "data" </> "static") (dataDir </> "static")

-- | Copy @\@bjorn3\/browser_wasi_shim@ dist files into the jsexe directory,
-- renaming @index.js@ to @wasi-shim.js@.
copyWasiShim :: FilePath -> IO ()
copyWasiShim jsexeDir = do
  shimPathM <- lookupEnv "OBELISK_WASI_SHIM"
  shimPath <- case shimPathM of
    Nothing -> fail "[Setup] OBELISK_WASI_SHIM not set. Set it to the @bjorn3/browser_wasi_shim package root."
    Just p -> pure p
  let distDir = shimPath </> "dist"
  distExists <- doesDirectoryExist distDir
  unless distExists $
    fail $ "[Setup] OBELISK_WASI_SHIM dist directory not found: " <> distDir
  files <- listDirectory distDir
  let jsFiles = filter (\f -> takeExtension f == ".js") files
  forM_ jsFiles $ \f -> do
    let dest = if f == "index.js" then "wasi-shim.js" else f
    copyFile (distDir </> f) (jsexeDir </> dest)
  hPutStrLn stderr $ "[Setup] Copied " <> show (length jsFiles) <> " wasi-shim files"

-- | Optionally run @wasm-opt@ and @wasm-tools strip@ if available on PATH.
optimizeWasm :: FilePath -> IO ()
optimizeWasm wasmFile = do
  wasmOpt <- findExecutable "wasm-opt"
  case wasmOpt of
    Nothing -> hPutStrLn stderr "[Setup] wasm-opt not on PATH; skipping optimization"
    Just _ -> do
      hPutStrLn stderr "[Setup] Running wasm-opt..."
      let optimized = wasmFile <> ".opt"
      withCreateProcess (proc "wasm-opt" ["-all", "-O2", wasmFile, "-o", optimized]) $ \_ _ _ ph -> do
        ec <- waitForProcess ph
        case ec of
          ExitSuccess -> do
            copyFile optimized wasmFile
            removeFile optimized
          ExitFailure _ ->
            hPutStrLn stderr "[Setup] wasm-opt failed; using unoptimized binary"
  wasmTools <- findExecutable "wasm-tools"
  case wasmTools of
    Nothing -> hPutStrLn stderr "[Setup] wasm-tools not on PATH; skipping strip"
    Just _ -> do
      hPutStrLn stderr "[Setup] Running wasm-tools strip..."
      let stripped = wasmFile <> ".stripped"
      withCreateProcess (proc "wasm-tools" ["strip", "-a", wasmFile, "-o", stripped]) $ \_ _ _ ph -> do
        ec <- waitForProcess ph
        case ec of
          ExitSuccess -> do
            copyFile stripped wasmFile
            removeFile stripped
          ExitFailure _ ->
            hPutStrLn stderr "[Setup] wasm-tools strip failed; continuing"

-- | Browser bootstrap shim for WASM frontend.
-- Canonical source: nix/wasm/shim.js
shimJs :: String
shimJs = unlines
  [ "(function() {"
  , "  var base = new URL(\".\", document.currentScript.src).href;"
  , "  (async function() {"
  , "    const { WASI, OpenFile, File, ConsoleStdout } = await import(base + \"wasi-shim.js\");"
  , "    const { default: ghc_wasm_jsffi } = await import(base + \"ghc_wasm_jsffi.js\");"
  , ""
  , "    const args = [];"
  , "    const env = [\"GHCRTS=-H64m\"];"
  , "    const fds = ["
  , "      new OpenFile(new File([])),"
  , "      ConsoleStdout.lineBuffered(msg => console.log(`[WASI stdout] ${msg}`)),"
  , "      ConsoleStdout.lineBuffered(msg => console.warn(`[WASI stderr] ${msg}`)),"
  , "    ];"
  , "    const wasi = new WASI(args, env, fds, { debug: false });"
  , ""
  , "    const instance_exports = {};"
  , "    const response = await fetch(base + \"frontend.wasm\");"
  , "    const bytes = await response.arrayBuffer();"
  , "    const { instance } = await WebAssembly.instantiate(bytes,"
  , "      { wasi_snapshot_preview1: wasi.wasiImport,"
  , "        ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports) }"
  , "    );"
  , "    Object.assign(instance_exports, instance.exports);"
  , "    wasi.initialize(instance);"
  , "    await instance.exports.hs_start();"
  , "  })();"
  , "})();"
  ]


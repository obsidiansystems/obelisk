module Backend where

import Obelisk.Backend
import System.FilePath ((</>))

import Common
import Frontend
import Paths (dataDir)



backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ const $ return ()
  , _backend_routeEncoder = fullRouteEncoder
  }

-- | Serve the app. This is the entry point used by the backend executable,
-- and by @ob-run@\/@ob-repl@ to run the server inside a GHCi session (where
-- it reloads in-process instead of relinking).
run :: IO ()
run = runBackendWith config backend frontend
  where
    config = BackendConfig
      { _backendConfig_runSnap = runSnapWithCommandLineArgs
      , _backendConfig_staticAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "static.assets"
          , _staticAssets_unprocessed = dataDir </> "static"
          }
      , _backendConfig_frontendGhcjsAssets = StaticAssets
          { _staticAssets_processed = dataDir </> "frontend.jsexe.assets"
          , _staticAssets_unprocessed = dataDir </> "frontend.jsexe"
          }
      , _backendConfig_ghcjsWidgets = defaultGhcjsWidgets
      }

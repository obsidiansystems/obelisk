module Dev where

import System.Directory (withCurrentDirectory)
import Obelisk.Run (defaultRunApp, run, runServeAsset)

import Backend
import Frontend

dev :: FilePath -> IO ()
dev rootDir = withCurrentDirectory rootDir $ run $ defaultRunApp backend frontend $ runServeAsset $ rootDir <> "/static"

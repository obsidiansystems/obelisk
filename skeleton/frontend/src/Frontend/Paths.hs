{-# LANGUAGE CPP #-}

module Frontend.Paths where

#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)

dataDir :: FilePath
dataDir = "data"

#else

import Data.FileEmbed ()



#ifdef NOT_REPL

import Paths_frontend qualified
import System.IO.Unsafe

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO Paths_frontend.getDataDir

#else

import Data.FileEmbed

dataDir :: FilePath
dataDir = $(makeRelativeToProject "data" >>= strToExp)

#endif

#endif

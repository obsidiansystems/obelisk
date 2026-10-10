{-# LANGUAGE CPP #-}

module Paths where

import Data.FileEmbed ()



#ifdef NOT_REPL

import Paths_backend qualified
import System.Directory
import System.IO.Unsafe

{-# NOINLINE dataDir #-}
dataDir :: FilePath
dataDir = unsafePerformIO getDataDir

getDataDir :: IO FilePath
getDataDir = Paths_backend.getDataDir

{-# NOINLINE dataFileName #-}
dataFileName :: FilePath -> FilePath
dataFileName = unsafePerformIO . getDataFileName

getDataFileName :: FilePath -> IO FilePath
getDataFileName = Paths_backend.getDataFileName

{-# NOINLINE temporaryDir #-}
temporaryDir :: FilePath
temporaryDir = unsafePerformIO getTemporaryDirectory

#else

import Data.FileEmbed

dataDir :: FilePath
dataDir = $(makeRelativeToProject "data" >>= strToExp)

getDataDir :: IO FilePath
getDataDir = pure dataDir

dataFileName :: FilePath -> FilePath
dataFileName f = dataDir <> "/" <> f

getDataFileName :: FilePath -> IO FilePath
getDataFileName = pure . dataFileName

temporaryDir :: FilePath
temporaryDir = $(makeRelativeToProject "tmp" >>= strToExp)

#endif

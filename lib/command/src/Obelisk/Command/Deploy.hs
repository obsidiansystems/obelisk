{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Deploy where

import Control.Monad
import System.Directory
import System.FilePath
import System.Posix.Files

import Obelisk.Command.Utils
import Obelisk.Command.Thunk

deployInit :: ThunkPtr -> FilePath -> FilePath -> FilePath -> [String] -> IO ()
deployInit thunkPtr configDir deployDir sshKeyPath hostnames = do
  createDirectoryIfMissing True deployDir
  hasConfigDir <- doesDirectoryExist configDir
  when hasConfigDir $ do
    cp
      [ "-r"
      , "-T"
      , configDir
      , deployDir </> "config"
      ]
  keyExists <- doesFileExist sshKeyPath
  when keyExists $ do
    target <- makeAbsolute sshKeyPath
    createSymbolicLink target (deployDir </> "ssh_key")
  createThunk (deployDir </> "src") thunkPtr
  writeFile (deployDir </> "backend_hosts") $ unlines hostnames
  initGit deployDir


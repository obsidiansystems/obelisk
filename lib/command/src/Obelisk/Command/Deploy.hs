{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Deploy where

import Control.Monad
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.Posix.Files
import System.Process

import Obelisk.App (MonadObelisk)
import Obelisk.Command.CLI (failWith)
import Obelisk.Command.Nix
import Obelisk.Command.Thunk
import Obelisk.Command.Utils

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

deployPush :: MonadObelisk m => FilePath -> m ()
deployPush deployPath = do
  liftIO $ callProcess "ssh-add" [deployPath </> "ssh_key"]
  host <- liftIO $ fmap (T.unpack . T.strip) $ T.readFile $ deployPath </> "backend_hosts"
  let srcPath = deployPath </> "src"
      build = do
        buildOutput <- nixBuild $ def
          { _nixBuildConfig_target = Target
            { _target_path = srcPath
            , _target_attr = Just "server"
            }
          , _nixBuildConfig_outLink = OutLink_None
          , _nixBuildConfig_args = pure $ Arg "hostName" host
          }
        return $ listToMaybe $ lines buildOutput
  result <- readThunk srcPath >>= \case
    Right (ThunkData_Packed _) -> do
      unpackThunk srcPath
      build `finally` packThunk srcPath "origin" -- get upstream
    Right (ThunkData_Checkout _) -> do
      liftIO (checkGitCleanStatus srcPath) >>= \case
        True -> do
          result <- build
          packThunk srcPath "origin" -- get upstream
          return result
        False -> do
          _ <- failWith $ T.pack $ "ob deploy push: ensure " <> srcPath <> " has no pending changes and latest is pushed upstream."
          return Nothing
    _ -> return Nothing
  liftIO $ forM_ result $ \res -> do
    callProcess "nix-copy-closure" ["-v", "--to", "root@" <> host, "--gzip", res]
    callProcess "ssh"
      [ "root@" <> host
      , unwords
          [ "nix-env -p /nix/var/nix/profiles/system --set " <> res
          , "&&"
          , "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
          ]
      ]
    isClean <- liftIO $ checkGitCleanStatus deployPath
    when (not isClean) $ liftIO $ do
      callProcess "git" ["-C", deployPath, "add", "--update"]
      callProcess "git" ["-C", deployPath, "commit", "-m", "New deployment"]

deployUpdate :: MonadObelisk m => FilePath -> m ()
deployUpdate deployPath = updateThunkToLatest $ deployPath </> "src"

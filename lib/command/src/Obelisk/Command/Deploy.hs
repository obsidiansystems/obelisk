{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Deploy where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath
import System.Posix.Files
import System.Process (delegate_ctlc, env, proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..), callProcessAndLogOutput, failWith, putLog, withSpinner)
import Obelisk.Command.Nix
import Obelisk.Command.Project
import Obelisk.Command.Thunk
import Obelisk.Command.Utils

deployInit :: MonadObelisk m => ThunkPtr -> FilePath -> FilePath -> FilePath -> [String] -> m ()
deployInit thunkPtr configDir deployDir sshKeyPath hostnames = do
  hasConfigDir <- liftIO $ do
    createDirectoryIfMissing True deployDir
    doesDirectoryExist configDir
  localKey <- liftIO (doesFileExist sshKeyPath) >>= \case
    False -> failWith $ T.pack $ "ob deploy init: file does not exist: " <> sshKeyPath
    True -> pure $ deployDir </> "ssh_key"
  callProcessAndLogOutput (Notice, Error) $ proc "cp" [sshKeyPath, localKey]
  liftIO $ setFileMode localKey $ ownerReadMode .|. ownerWriteMode
  liftIO $ writeFile (deployDir </> "backend_hosts") $ unlines hostnames
  forM_ hostnames $ \hostname -> do
    putLog Notice $ "Verifying host keys (" <> T.pack hostname <> ")"
    verifyHostKey (deployDir </> "backend_known_hosts") localKey hostname
  when hasConfigDir $ do
    callProcessAndLogOutput (Notice, Error) $ proc "cp"
      [ "-r"
      , "-T"
      , configDir
      , deployDir </> "config"
      ]
  liftIO $ createThunk (deployDir </> "src") thunkPtr
  liftIO $ setupObeliskImpl deployDir
  initGit deployDir

setupObeliskImpl :: FilePath -> IO ()
setupObeliskImpl deployDir = do
  let implDir = toImplDir deployDir
  createDirectoryIfMissing True implDir
  writeFile (implDir </> "default.nix") "(import ../../src {}).obelisk"

deployPush :: MonadObelisk m => FilePath -> m [String] -> m ()
deployPush deployPath getNixBuilders = do
  let backendHosts = deployPath </> "backend_hosts"
  host <- liftIO $ fmap (T.unpack . T.strip) $ T.readFile backendHosts
  let srcPath = deployPath </> "src"
      build = do
        builders <- getNixBuilders
        buildOutput <- nixBuild $ def
          { _nixBuildConfig_target = Target
            { _target_path = srcPath
            , _target_attr = Just "server"
            }
          , _nixBuildConfig_outLink = OutLink_None
          , _nixBuildConfig_args = pure $ Arg "hostName" host
          , _nixBuildConfig_builders = builders
          }
        return $ listToMaybe $ lines buildOutput
  result <- readThunk srcPath >>= \case
    Right (ThunkData_Packed _) ->
      build
    Right (ThunkData_Checkout _) -> do
      checkGitCleanStatus srcPath True >>= \case
        True -> do
          result <- build
          packThunk srcPath "origin" -- get upstream
          return result
        False -> do
          _ <- failWith $ T.pack $ "ob deploy push: ensure " <> srcPath <> " has no pending changes and latest is pushed upstream."
          return Nothing
    _ -> return Nothing
  forM_ result $ \res -> do
    let knownHostsPath = deployPath </> "backend_known_hosts"
        sshOpts = sshArgs knownHostsPath (deployPath </> "ssh_key") False
        deployAndSwitch outputPath = do
          withSpinner "Uploading closure" $ do
            callProcess'
              (Map.fromList [("NIX_SSHOPTS", unwords sshOpts)])
              "nix-copy-closure" ["-v", "--to", "root@" <> host, "--gzip", outputPath]

          withSpinner "Switching to new configuration" $ do
            callProcessAndLogOutput (Notice, Warning) $
              proc "ssh" $ sshOpts <>
                [ "root@" <> host
                , unwords
                    [ "nix-env -p /nix/var/nix/profiles/system --set " <> outputPath
                    , "&&"
                    , "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                    ]
                ]
    deployAndSwitch res
    isClean <- checkGitCleanStatus deployPath True
    when (not isClean) $ do
      withSpinner "Commiting changes to Git" $ do
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "add", "."]
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "commit", "-m", "New deployment"]
  where
    callProcess' envMap cmd args = do
      processEnv <- Map.toList . (envMap <>) . Map.fromList <$> liftIO getEnvironment
      let p = (proc cmd args) { delegate_ctlc = True, env = Just processEnv }
      callProcessAndLogOutput (Notice, Notice) p

deployUpdate :: MonadObelisk m => FilePath -> m ()
deployUpdate deployPath = updateThunkToLatest $ deployPath </> "src"

deployMobile :: MonadObelisk m => String -> [String] -> m ()
deployMobile platform mobileArgs = withProjectRoot "." $ \root -> do
  let srcDir = root </> "src"
  exists <- liftIO $ doesDirectoryExist srcDir
  unless exists $ failWith "ob test should be run inside of a deploy directory"
  result <- nixBuildAttrWithCache srcDir $ platform <> ".frontend"
  callProcessAndLogOutput (Notice, Error) $ proc (result </> "bin" </> "deploy") mobileArgs

verifyHostKey :: MonadObelisk m => FilePath -> FilePath -> String -> m ()
verifyHostKey knownHostsPath keyPath hostName =
  callProcessAndLogOutput (Notice, Warning) $ proc "ssh" $
    sshArgs knownHostsPath keyPath True <>
      [ "root@" <> hostName
      , "exit"
      ]

sshArgs :: FilePath -> FilePath -> Bool -> [String]
sshArgs knownHostsPath keyPath askHostKeyCheck =
  [ "-o", "UserKnownHostsFile=" <> knownHostsPath
  , "-o", "StrictHostKeyChecking=" <> if askHostKeyCheck then "ask" else "yes"
  , "-i", keyPath
  ]

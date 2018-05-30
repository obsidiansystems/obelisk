{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Deploy where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (SomeException, finally)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8 as A
import Data.Bits
import qualified Data.ByteString.Char8 as BC8
import Data.Default
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.Posix.Env (getEnvironment)
import System.Posix.Files
import System.Process (delegate_ctlc, env, proc, readProcess)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..), callProcess, callProcessAndLogOutput, failWith, putLog, withSpinner)
import Obelisk.Command.Nix
import Obelisk.Command.Project
import Obelisk.Command.Thunk
import Obelisk.Command.Utils
import Obelisk.ExecutableConfig.Types (Route, getConfig, getRoutePort)

deployInit :: MonadObelisk m => ThunkPtr -> FilePath -> FilePath -> FilePath -> [String] -> m ()
deployInit thunkPtr configDir deployDir sshKeyPath hostnames = do
  hasConfigDir <- liftIO $ do
    createDirectoryIfMissing True deployDir
    doesDirectoryExist configDir
  when hasConfigDir $ do
    callProcessAndLogOutput (Notice, Error) $
      cp
        [ "-r"
        , "-T"
        , configDir
        , deployDir </> "config"
        ]
  keyExists <- liftIO $ doesFileExist sshKeyPath
  when keyExists $ do
    let localKey = deployDir </> "ssh_key"
    callProcessAndLogOutput (Notice, Error) $
      cp [sshKeyPath, localKey]
    liftIO $ setFileMode localKey $ ownerReadMode .|. ownerWriteMode
    liftIO $ writeFile (deployDir </> "backend_hosts") $ unlines hostnames
    forM_ hostnames $ \hostname -> do
      putLog Notice $ "Verifying host keys (" <> T.pack hostname <> ")"
      verifyHostKey (deployDir </> "backend_known_hosts") localKey hostname
  liftIO $ createThunk (deployDir </> "src") thunkPtr
  liftIO $ setupObeliskImpl deployDir
  initGit deployDir

setupObeliskImpl :: FilePath -> IO ()
setupObeliskImpl deployDir = do
  let implDir = toImplDir deployDir
  createDirectoryIfMissing True implDir
  writeFile (implDir </> "default.nix") "(import ../../src {}).obelisk"

-- | Verify configuration files and return the config values.
deployVerify :: MonadObelisk m => FilePath -> m Route
deployVerify deployPath = do
  getConfig deployPath >>= \case
    Left e -> failWith $ T.pack $ "common/route: " <> show (e :: SomeException)
    Right v -> putLog Debug (T.pack $ "common/route: verified") >> pure v

deployPush :: MonadObelisk m => FilePath -> m ()
deployPush deployPath = do
  let backendHosts = deployPath </> "backend_hosts"
  route <- deployVerify deployPath
  let (Just port) = getRoutePort route
  putLog Debug $ T.pack $ "Deploying with route: " <> show route

  host <- liftIO $ fmap (T.unpack . T.strip) $ T.readFile backendHosts
  let srcPath = deployPath </> "src"
      build = do
        buildOutput <- nixBuild $ def
          { _nixBuildConfig_target = Target
            { _target_path = srcPath
            , _target_attr = Just "server"
            }
          , _nixBuildConfig_outLink = OutLink_None
          , _nixBuildConfig_args =
            [ Arg "hostName" host
            , Arg "backendPort" $ show port
            ]
          }
        return $ listToMaybe $ lines buildOutput
  result <- readThunk srcPath >>= \case
    Right (ThunkData_Packed _) ->
      build
    Right (ThunkData_Checkout _) -> do
      checkGitCleanStatus srcPath >>= \case
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
        sshOpts = ["-o", "UserKnownHostsFile=" <> knownHostsPath, "-o", "StrictHostKeyChecking=yes"]
        deployAndSwitch outputPath sshAgentEnv = do
          withSpinner "Adding ssh key" $ do
            callProcess' sshAgentEnv "ssh-add" [deployPath </> "ssh_key"]
          withSpinner "Uploading closure" $ do
            let nixSshEnv = ("NIX_SSHOPTS", unwords sshOpts) : sshAgentEnv
            callProcess' nixSshEnv "nix-copy-closure" ["-v", "--to", "root@" <> host, "--gzip", outputPath]
          withSpinner "Switching to new configuration" $ do
            callProcess' sshAgentEnv "ssh" $ sshOpts <>
              [ "root@" <> host
              , unwords
                  [ "nix-env -p /nix/var/nix/profiles/system --set " <> outputPath
                  , "&&"
                  , "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                  ]
              ]
    sshAgentEnv <- liftIO sshAgent
    deployAndSwitch res sshAgentEnv `finally` callProcess' sshAgentEnv "ssh-agent" ["-k"]
    isClean <- checkGitCleanStatus deployPath
    when (not isClean) $ do
      withSpinner "Commiting changes to Git" $ do
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "add", "."]
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "commit", "-m", "New deployment"]
  where
    callProcess' e cmd args = do
      currentEnv <- liftIO getEnvironment
      let p = (proc cmd args) { delegate_ctlc = True, env = Just $ e <> currentEnv }
      callProcessAndLogOutput (Notice, Notice) p

deployUpdate :: MonadObelisk m => FilePath -> m ()
deployUpdate deployPath = updateThunkToLatest $ deployPath </> "src"

deployMobile :: MonadObelisk m => String -> [String] -> m ()
deployMobile platform mobileArgs = withProjectRoot "." $ \root -> do
  let srcDir = root </> "src"
  exists <- liftIO $ doesDirectoryExist srcDir
  unless exists $ failWith "ob test should be run inside of a deploy directory"
  result <- nixBuildAttrWithCache srcDir $ platform <> ".frontend"
  callProcess (result </> "bin" </> "deploy") mobileArgs

sshAgent :: IO [(String, String)]
sshAgent = do
  output <- BC8.pack <$> readProcess "ssh-agent" ["-c"] mempty
  return $ fromMaybe mempty $ A.maybeResult $ A.parse (A.many' p) output
  where
    p = do
      key <- A.string "setenv" >> A.skipSpace >> A.takeWhile (not . isSpace)
      val <- A.skipSpace >> A.takeWhile (/= ';')
      _ <- A.string ";" >> A.endOfLine <|> void (A.string ";")
      return (BC8.unpack key, BC8.unpack val)

verifyHostKey :: MonadObelisk m => FilePath -> FilePath -> String -> m ()
verifyHostKey knownHostsPath keyPath hostName =
  callProcessAndLogOutput (Notice, Warning) $ proc "ssh"
    [ "-o", "UserKnownHostsFile=" <> knownHostsPath
    , "-o", "StrictHostKeyChecking=ask"
    , "-i", keyPath
    , "root@" <> hostName
    , "exit"
    ]

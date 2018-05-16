{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Deploy where

import Control.Applicative
import Control.Monad
import Control.Monad.Catch (finally)
import Control.Monad.IO.Class (liftIO)
import Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as BC8
import Data.Default
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.FilePath
import System.Process (delegate_ctlc, env, proc, readProcess)

import Obelisk.App (MonadObelisk)
import Obelisk.CLI (Severity (..), callProcessAndLogOutput, failWith, withSpinner)
import Obelisk.Command.Nix
import Obelisk.Command.Thunk
import Obelisk.Command.Utils

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
    liftIO $ setPermissions localKey $ setOwnerWritable True $ setOwnerReadable True emptyPermissions
  liftIO $ do
    createThunk (deployDir </> "src") thunkPtr
    writeFile (deployDir </> "backend_hosts") $ unlines hostnames
    initGit deployDir

deployPush :: MonadObelisk m => FilePath -> m ()
deployPush deployPath = do
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
    Right (ThunkData_Packed _) ->
      build
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
  forM_ result $ \res -> do
    let deployAndSwitch outputPath sshAgentEnv = do
          withSpinner "Adding ssh key" $ do
            callProcess' (Just sshAgentEnv) "ssh-add" [deployPath </> "ssh_key"]
          withSpinner "Uploading closure" $ do
            callProcess' (Just sshAgentEnv) "nix-copy-closure" ["-v", "--to", "root@" <> host, "--gzip", outputPath]
          withSpinner "Switching to new configuration" $ do
            callProcess' (Just sshAgentEnv) "ssh"
              [ "root@" <> host
              , unwords
                  [ "nix-env -p /nix/var/nix/profiles/system --set " <> outputPath
                  , "&&"
                  , "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                  ]
              ]
    sshAgentEnv <- liftIO sshAgent
    deployAndSwitch res sshAgentEnv `finally` callProcess' (Just sshAgentEnv) "ssh-agent" ["-k"]
    isClean <- liftIO $ checkGitCleanStatus deployPath
    when (not isClean) $ do
      withSpinner "Commiting changes to Git" $ do
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "add", "--update"]
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "commit", "-m", "New deployment"]
  where
    callProcess' e cmd args = do
      let p = (proc cmd args) { delegate_ctlc = True, env = e }
      callProcessAndLogOutput (Notice, Notice) p

deployUpdate :: MonadObelisk m => FilePath -> m ()
deployUpdate deployPath = updateThunkToLatest $ deployPath </> "src"

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


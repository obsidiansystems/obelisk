{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Deploy where

import Control.Applicative
import Control.Exception
import Control.Monad
import qualified Data.ByteString.Char8 as BC8
import Data.Attoparsec.ByteString.Char8 as A
import Data.Default
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Exit (ExitCode(..))
import System.FilePath
import System.Process

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
    let localKey = deployDir </> "ssh_key"
    cp [sshKeyPath, localKey]
    setPermissions localKey $ setOwnerWritable True $ setOwnerReadable True emptyPermissions
  createThunk (deployDir </> "src") thunkPtr
  writeFile (deployDir </> "backend_hosts") $ unlines hostnames
  initGit deployDir

deployPush :: FilePath -> IO ()
deployPush deployPath = do
  host <- fmap (T.unpack . T.strip) $ T.readFile $ deployPath </> "backend_hosts"
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
        return $ listToMaybe $ lines $ buildOutput
  result <- readThunk srcPath >>= \case
    Right (ThunkData_Packed _) -> do
      unpackThunk srcPath
      result <- build `finally` packThunk srcPath "origin" -- get upstream
      return result
    Right (ThunkData_Checkout _) -> do
      checkGitCleanStatus srcPath >>= \case
        True -> do
          result <- build
          packThunk srcPath "origin" -- get upstream
          return result
        False -> do
          _ <- fail $ "ob deploy push: ensure " <> srcPath <> " has no pending changes and latest is pushed upstream."
          return Nothing
    _ -> return Nothing
  forM_ result $ \res -> do
    let deployAndSwitch outputPath sshAgentEnv = do
          callProcess' (Just sshAgentEnv) "ssh-add" [deployPath </> "ssh_key"]
          callProcess' (Just sshAgentEnv) "nix-copy-closure" ["-v", "--to", "root@" <> host, "--gzip", outputPath]
          callProcess' (Just sshAgentEnv) "ssh"
            [ "root@" <> host
            , unwords
                [ "nix-env -p /nix/var/nix/profiles/system --set " <> outputPath
                , "&&"
                , "/nix/var/nix/profiles/system/bin/switch-to-configuration switch"
                ]
            ]
    sshAgentEnv <- sshAgent
    deployAndSwitch res sshAgentEnv `finally` callProcess' (Just sshAgentEnv) "ssh-agent" ["-k"]
    isClean <- checkGitCleanStatus deployPath
    when (not isClean) $ do
      callProcess "git" ["-C", deployPath, "add", "--update"]
      callProcess "git" ["-C", deployPath, "commit", "-m", "New deployment"]
  where
    callProcess' e cmd args = do
      let p = (proc cmd args) { delegate_ctlc = True, env = e }
      exit_code <- withCreateProcess p $ \_ _ _ ph -> waitForProcess ph
      case exit_code of
        ExitSuccess -> return ()
        ExitFailure r -> fail $ "callProcess'" <> "(exit: " <> show r <> ")"

deployUpdate :: FilePath -> IO ()
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


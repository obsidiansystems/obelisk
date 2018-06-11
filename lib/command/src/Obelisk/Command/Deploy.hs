{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Deploy where

import Control.Lens
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
import qualified Text.URI as URI
import Text.URI.Lens

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp (Severity (..), callProcessAndLogOutput, failWith, putLog, withSpinner)
import Obelisk.Command.Nix
import Obelisk.Command.Project
import Obelisk.Command.Thunk
import Obelisk.Command.Utils

deployInit
  :: MonadObelisk m
  => ThunkPtr
  -> FilePath
  -> FilePath
  -> FilePath
  -> [String] -- ^ hostnames
  -> String -- ^ route
  -> String -- ^ admin email
  -> Bool -- ^ disable https
  -> m ()
deployInit thunkPtr configDir deployDir sshKeyPath hostnames route adminEmail disableHttps = do
  hasConfigDir <- withSpinner ("Preparing " <> T.pack deployDir) $ liftIO $ do
    createDirectoryIfMissing True deployDir
    doesDirectoryExist configDir
  localKey <- liftIO (doesFileExist sshKeyPath) >>= \case
    False -> failWith $ T.pack $ "ob deploy init: file does not exist: " <> sshKeyPath
    True -> pure $ deployDir </> "ssh_key"
  unless disableHttps $ do
    void $ getSslHostFromRoute route -- make sure that hostname is present
  callProcessAndLogOutput (Notice, Error) $
    cp [sshKeyPath, localKey]
  liftIO $ setFileMode localKey $ ownerReadMode .|. ownerWriteMode
  forM_ hostnames $ \hostname -> do
    putLog Notice $ "Verifying host keys (" <> T.pack hostname <> ")"
    -- Note: we can't use a spinner here as this function will prompt the user.
    verifyHostKey (deployDir </> "backend_known_hosts") localKey hostname
  when hasConfigDir $ withSpinner "Importing project configuration" $ do
    callProcessAndLogOutput (Notice, Error) $
      cp [ "-r" , "-T" , configDir , deployDir </> "config"]
  withSpinner "Writing deployment configuration" $ do
    writeDeployConfig deployDir "backend_hosts" $ unlines hostnames
    writeDeployConfig deployDir "disable_https" $ show disableHttps
    writeDeployConfig deployDir "admin_email" adminEmail
    writeDeployConfig deployDir ("config" </> "common" </> "route") $ route
  withSpinner "Creating source thunk" $ liftIO $ do
    createThunk (deployDir </> "src") thunkPtr
    setupObeliskImpl deployDir
  withSpinner "Initializing git repository" $
    initGit deployDir

setupObeliskImpl :: FilePath -> IO ()
setupObeliskImpl deployDir = do
  let implDir = toImplDir deployDir
  createDirectoryIfMissing True implDir
  writeFile (implDir </> "default.nix") "(import ../../src {}).obelisk"

deployPush :: MonadObelisk m => FilePath -> m [String] -> m ()
deployPush deployPath getNixBuilders = do
  host <- readDeployConfig deployPath "backend_hosts"
  adminEmail <- readDeployConfig deployPath "admin_email"
  disableHttps <- read <$> readDeployConfig deployPath "disable_https"
  route <- readDeployConfig deployPath $ "config" </> "common" </> "route"
  sslHost <- if disableHttps then pure Nothing else Just <$> getSslHostFromRoute route
  let srcPath = deployPath </> "src"
      build = do
        builders <- getNixBuilders
        buildOutput <- nixBuild $ def
          { _nixBuildConfig_target = Target
            { _target_path = srcPath
            , _target_attr = Just "server"
            }
          , _nixBuildConfig_outLink = OutLink_None
          , _nixBuildConfig_args =
            [ Arg "hostName" host
            , Arg "adminEmail" adminEmail
            , Arg "sslHost" $ fromMaybe "" sslHost
            ]
          , _nixBuildConfig_builders = builders
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
    isClean <- checkGitCleanStatus deployPath
    when (not isClean) $ do
      withSpinner "Commiting changes to Git" $ do
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "add", "."]
        callProcessAndLogOutput (Debug, Error) $ proc "git"
          ["-C", deployPath, "commit", "-m", "New deployment"]
    putLog Notice $
      (if disableHttps then "Deployed (without https) => " else "Deployed => ")
      <> T.pack route
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

-- | Simplified deployment configuration mechanism. At one point we may revisit this.
writeDeployConfig :: MonadObelisk m => FilePath -> FilePath -> String -> m ()
writeDeployConfig deployDir fname = liftIO . writeFile (deployDir </> fname)

readDeployConfig :: MonadObelisk m => FilePath -> FilePath -> m String
readDeployConfig deployDir fname = liftIO $ do
  fmap (T.unpack . T.strip) $ T.readFile $ deployDir </> fname

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

-- | Get the hostname from a ssl route
--
-- Fail if the route is invalid (i.e, no host present or scheme is not https)
getSslHostFromRoute :: MonadObelisk m => String -> m String
getSslHostFromRoute route = do
  uri <- URI.mkURI $ T.strip $ T.pack route
  -- TODO: find a way to simplify this code
  case uri ^? uriScheme of
    Nothing -> failWith "Missing scheme (relative route) in route"
    Just scheme -> case scheme of
      Just s -> case URI.unRText s of
        "https" -> pure ()
        _ -> failWith $ "Invalid scheme (not https)" -- <> URI.unRText s
      _ -> failWith $ "Missing scheme in route" -- <> URI.unRText s
  case uri ^? uriAuthority . _Right . authHost of
    Nothing -> failWith "Missing hostname in route"
    Just sslHost -> return $ T.unpack $ URI.unRText sslHost

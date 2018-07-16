{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Obelisk.Command.Deploy where

import Control.Lens
import Control.Monad
import Control.Monad.Catch (Exception (displayException), MonadThrow, throwM, try)
import Control.Monad.IO.Class (liftIO)
import Data.Bits
import Data.Default
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment (getEnvironment)
import System.FilePath
import System.Posix.Files
import System.Process (delegate_ctlc, env, proc)
import Text.URI (URI)
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
  -> Bool -- ^ enable https
  -> m ()
deployInit thunkPtr configDir deployDir sshKeyPath hostnames route adminEmail enableHttps = do
  (hasConfigDir, localKey) <- withSpinner ("Preparing " <> T.pack deployDir) $ do
    hasConfigDir <- liftIO $ do
      createDirectoryIfMissing True deployDir
      doesDirectoryExist configDir
    localKey <- liftIO (doesFileExist sshKeyPath) >>= \case
      False -> failWith $ T.pack $ "ob deploy init: file does not exist: " <> sshKeyPath
      True -> pure $ deployDir </> "ssh_key"
    callProcessAndLogOutput (Notice, Error) $
      proc "cp" [sshKeyPath, localKey]
    liftIO $ setFileMode localKey $ ownerReadMode .|. ownerWriteMode
    return $ (hasConfigDir, localKey)
  withSpinner "Validating configuration" $ do
    void $ getHostFromRoute enableHttps route -- make sure that hostname is present
  forM_ hostnames $ \hostname -> do
    putLog Notice $ "Verifying host keys (" <> T.pack hostname <> ")"
    -- Note: we can't use a spinner here as this function will prompt the user.
    verifyHostKey (deployDir </> "backend_known_hosts") localKey hostname
  when hasConfigDir $ withSpinner "Importing project configuration" $ do
    callProcessAndLogOutput (Notice, Error) $
      proc "cp" [ "-r" , "-T" , configDir , deployDir </> "config"]
  withSpinner "Writing deployment configuration" $ do
    writeDeployConfig deployDir "backend_hosts" $ unlines hostnames
    writeDeployConfig deployDir "enable_https" $ show enableHttps
    writeDeployConfig deployDir "admin_email" adminEmail
    writeDeployConfig deployDir ("config" </> "common" </> "route") $ route
  withSpinner "Creating source thunk (./src)" $ liftIO $ do
    createThunk (deployDir </> "src") thunkPtr
    setupObeliskImpl deployDir
  withSpinner ("Initializing git repository (" <> T.pack deployDir <> ")") $
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
  enableHttps <- read <$> readDeployConfig deployPath "enable_https"
  route <- readDeployConfig deployPath $ "config" </> "common" </> "route"
  routeHost <- getHostFromRoute enableHttps route
  let srcPath = deployPath </> "src"
      build = do
        builders <- getNixBuilders
        buildOutput <- nixBuild $ def
          { _nixBuildConfig_target = Target
            { _target_path = srcPath
            , _target_attr = Just "server.system"
            }
          , _nixBuildConfig_outLink = OutLink_None
          , _nixBuildConfig_args =
            [ strArg "hostName" host
            , strArg "adminEmail" adminEmail
            , strArg "routeHost" routeHost
            , boolArg "enableHttps" enableHttps
            ]
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
    putLog Notice $ "Deployed => " <> T.pack route
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

-- common/route validation
-- TODO: move these to executable-config once the typed-config stuff is done.

data InvalidRoute
  = InvalidRoute_NotHttps URI
  | InvalidRoute_MissingScheme URI
  | InvalidRoute_MissingHost URI
  | InvalidRoute_HasPort URI
  | InvalidRoute_HasPath URI
  deriving Show

instance Exception InvalidRoute where
  displayException = \case
    InvalidRoute_MissingScheme uri -> route uri "must have an URI scheme"
    InvalidRoute_NotHttps uri -> route uri "must be HTTPS"
    InvalidRoute_MissingHost uri -> route uri "must contain a hostname"
    InvalidRoute_HasPort uri -> route uri "cannot specify port"
    InvalidRoute_HasPath uri -> route uri "cannot contain path"
    where
      route uri err = T.unpack $ "Route (" <> URI.render uri <> ") " <> err

-- | Get the hostname from a https route
--
-- Fail if the route is invalid (i.e, no host present or scheme is not https)
getHostFromRoute
  :: MonadObelisk m
  => Bool  -- ^ Ensure https?
  -> String
  -> m String
getHostFromRoute mustBeHttps route = do
  result :: Either InvalidRoute String <- try $ do
    validateCommonRouteAndGetHost mustBeHttps =<< URI.mkURI (T.strip $ T.pack route)
  either (failWith . T.pack . displayException) pure result

validateCommonRouteAndGetHost
  :: (MonadThrow m, MonadObelisk m)
  => Bool -- ^ Ensure https?
  -> URI
  -> m String
validateCommonRouteAndGetHost mustBeHttps uri = do
  case uri ^? uriScheme of
    Just (Just (URI.unRText -> s)) -> case (mustBeHttps, s) of
      (False, _) -> pure ()
      (True, "https") -> pure ()
      _ -> throwM $ InvalidRoute_NotHttps uri
    _ -> throwM $ InvalidRoute_MissingScheme uri
  case uri ^. uriPath of
    [] -> pure ()
    _path -> throwM $ InvalidRoute_HasPath uri
  case uri ^? uriAuthority . _Right . authPort of
    Just (Just _port) -> throwM $ InvalidRoute_HasPort uri
    _ -> pure ()
  case uri ^? uriAuthority . _Right . authHost of
    Nothing -> throwM $ InvalidRoute_MissingHost uri
    Just sslHost -> return $ T.unpack $ URI.unRText sslHost

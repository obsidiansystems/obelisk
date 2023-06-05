{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ViewPatterns #-}
{-|
   Description:
   Implementation of the CLI deploy commands. Deployment is done by intializing
   a staging area for deployment configuration, and then by actually executing
   the deployment by installing a NixOS configuration at the configured deployment
   locations.
-}
module Obelisk.Command.Deploy where

import Control.Applicative (liftA2)
import Control.Lens
import Control.Monad
import Control.Monad.Catch (Exception (displayException), MonadThrow, bracket, throwM, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON, encode, eitherDecode)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Default
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String.Here.Interpolated (i)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import System.Directory
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath
import System.IO
import System.Which
import System.PosixCompat.Files
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.Lens
import Nix.Pretty (prettyNix)
import qualified Nix.Expr.Shorthands as Nix
import Prettyprinter (layoutCompact)
import Prettyprinter.Render.String (renderString)

import Obelisk.App (MonadObelisk, wrapNixThunkError)
import Obelisk.Command.Nix
import Obelisk.Command.Project
import Obelisk.Command.Utils

import "nix-thunk" Nix.Thunk
import Cli.Extras

-- | Options passed to the `init` verb
data DeployInitOpts = DeployInitOpts
  { _deployInitOpts_outputDir :: FilePath
  -- ^ Where to set up the deployment staging area
  , _deployInitOpts_sshKey :: FilePath
  -- ^ Which SSH Key will be used to interface with the deployment hosts
  , _deployInitOpts_hostname :: [String]
  -- ^ The hostnames that locate the deployment hosts
  , _deployInitOpts_route :: String
  -- ^ The route they are serving
  , _deployInitOpts_adminEmail :: String
  -- ^ The administrator email, for ACME
  , _deployInitOpts_enableHttps :: Bool
  -- ^ Whether or not to use HTTPS, which entails using Lets Encrypt by default
  , _deployInitOpts_checkKnownHosts :: Bool
  -- ^ Whether or not to use known_hosts file when assessing the identity of the deployment hosts
  } deriving Show

-- | The `init` verb
deployInit
  :: MonadObelisk m
  => DeployInitOpts
  -- ^ Command line arguments
  -> FilePath
  -- ^ Project root, which cannot be the same as the deployment dir
  -> m ()
deployInit deployOpts root = do
  let deployDir = _deployInitOpts_outputDir deployOpts
  rootEqualsTarget <- liftIO $ liftA2 equalFilePath (canonicalizePath root) (canonicalizePath deployDir)
  when rootEqualsTarget $
    failWith [i|Deploy directory ${deployDir} should not be the same as project root.|]
  thunkPtr <- wrapNixThunkError (readThunk root) >>= \case
    Right (ThunkData_Packed _ ptr) -> return ptr
    _ -> wrapNixThunkError (getThunkPtr CheckClean_NotIgnored root Nothing)
  deployInit' thunkPtr deployOpts

-- | The preamble in 'deployInit' provides deployInit' with a 'ThunkPtr' that it can install in
-- the staging directory.
deployInit'
  :: MonadObelisk m
  => ThunkPtr
  -> DeployInitOpts
  -> m ()
deployInit' thunkPtr (DeployInitOpts deployDir sshKeyPath hostnames route adminEmail enableHttps checkKnownHosts) = do
  liftIO $ createDirectoryIfMissing True deployDir
  localKey <- withSpinner ("Preparing " <> T.pack deployDir) $ do
    localKey <- liftIO (doesFileExist sshKeyPath) >>= \case
      False -> failWith $ T.pack $ "ob deploy init: file does not exist: " <> sshKeyPath
      True -> pure $ deployDir </> "ssh_key"
    callProcessAndLogOutput (Notice, Error) $
      proc cp [sshKeyPath, localKey]
    liftIO $ setFileMode localKey $ ownerReadMode .|. ownerWriteMode
    return localKey
  withSpinner "Validating configuration" $ do
    void $ getHostFromRoute enableHttps route -- make sure that hostname is present
  let obKnownHostsPath = deployDir </> "backend_known_hosts"
  forM_ hostnames $ \hostname -> do
    putLog Notice $ "Verifying host keys (" <> T.pack hostname <> ")"
    -- Note: we can't use a spinner here as this function will prompt the user.
    when checkKnownHosts $ addKnownHostFromEnv hostname obKnownHostsPath
    verifyHostKey obKnownHostsPath localKey hostname
  --IMPORTANT: We cannot copy config directory from the development project to
  --the deployment directory.  If we do, it's very likely someone will
  --accidentally create a production deployment that uses development
  --credentials to connect to some resources.  This could result in, e.g.,
  --production data backed up to a dev environment.
  withSpinner "Creating project configuration directories" $ liftIO $ do
    mapM_ (createDirectoryIfMissing True)
      [ deployDir </> "config" </> "backend"
      , deployDir </> "config" </> "common"
      , deployDir </> "config" </> "frontend"
      ]

  let srcDir = deployDir </> "src"
  withSpinner ("Creating source thunk (" <> T.pack (makeRelative deployDir srcDir) <> ")") $ do
    wrapNixThunkError . createThunk srcDir $ Right thunkPtr
    setupObeliskImpl deployDir

  withSpinner "Writing deployment configuration" $ do
    writeDeployConfig deployDir "backend_hosts" $ unlines hostnames
    writeDeployConfig deployDir "enable_https" $ show enableHttps
    writeDeployConfig deployDir "admin_email" adminEmail
    writeDeployConfig deployDir ("config" </> "common" </> "route") route
    writeDeployConfig deployDir "module.nix" $
      "(import " <> toNixPath (makeRelative deployDir srcDir) <> " {}).obelisk.serverModules.mkBaseEc2"

  withSpinner ("Initializing git repository (" <> T.pack deployDir <> ")") $
    initGit deployDir

-- | Installs an obelisk impl in the staging dir that points at the obelisk of the
-- project thunk.
setupObeliskImpl :: MonadIO m => FilePath -> m ()
setupObeliskImpl deployDir = liftIO $ do
  let
    implDir = toImplDir deployDir
    goBackUp = foldr (</>) "" $ (".." <$) $ splitPath $ makeRelative deployDir implDir
  createDirectoryIfMissing True implDir
  writeFile (implDir </> "default.nix") $ "(import " <> toNixPath (goBackUp </> "src") <> " {}).obelisk"

-- | Executes the deployment specified in the supplied staging dir
deployPush
  :: MonadObelisk m
  => FilePath
  -- ^ Path to the staging directory
  -> [String]
  -- ^ nix builders arg string for the nix-build that builds the deployment artefacts
  -> m ()
deployPush deployPath builders = do
  hosts <- Set.fromList . filter (/= mempty) . lines <$> readDeployConfig deployPath "backend_hosts"
  adminEmail <- readDeployConfig deployPath "admin_email"
  enableHttps <- read <$> readDeployConfig deployPath "enable_https"
  route <- readDeployConfig deployPath $ "config" </> "common" </> "route"
  routeHost <- getHostFromRoute enableHttps route
  redirectHosts <- liftIO (doesFileExist "redirect_hosts") >>= \case
    True -> Set.fromList . filter (/= mempty) . lines <$> readDeployConfig deployPath "redirect_hosts"
    False -> pure mempty
  let srcPath = deployPath </> "src"
  thunkPtr <- wrapNixThunkError (readThunk srcPath) >>= \case
    Right (ThunkData_Packed _ ptr) -> return ptr
    Right ThunkData_Checkout -> do
      checkGitCleanStatus srcPath True >>= \case
        True -> wrapNixThunkError $ packThunk (ThunkPackConfig False (ThunkConfig Nothing)) srcPath
        False -> failWith $ T.pack $ "ob deploy push: ensure " <> srcPath <> " has no pending changes and latest is pushed upstream."
    Left err -> failWith $ "ob deploy push: couldn't read src thunk: " <> T.pack (show err)
  let version = show . _thunkRev_commit $ _thunkPtr_rev thunkPtr
  let moduleFile = deployPath </> "module.nix"
  moduleFileExists <- liftIO $ doesFileExist moduleFile

  configHash <- getGitHash deployPath "config"
  buildOutputByHost <- ifor (Map.fromSet (const ()) hosts) $ \host () -> do
    --TODO: What does it mean if this returns more or less than 1 line of output?
    [result] <- fmap lines $ nixCmd $ NixCmd_Build $ def
      & nixCmdConfig_target .~ Target
        { _target_path = Just srcPath
        , _target_attr = Just "server.system"
        , _target_expr = Nothing
        }
      & nixBuildConfig_outLink .~ OutLink_None
      & nixCmdConfig_args .~ (
        [ strArg "hostName" $ fmap (\c -> if c == '.' then '_' else c) host
        , strArg "adminEmail" adminEmail
        , strArg "routeHost" routeHost
        , rawArg "redirectHosts" $ renderString $ layoutCompact $ prettyNix $ Nix.mkList $ Nix.mkStr . T.pack <$> Set.toList redirectHosts
        , strArg "version" version
        , boolArg "enableHttps" enableHttps
        , strArg "configHash" $ T.unpack $ T.strip (_gitHash_text configHash)
        ] <> [rawArg "module" ("import " <> toNixPath moduleFile) | moduleFileExists ])
      & nixCmdConfig_builders .~ builders
    pure result
  let knownHostsPath = deployPath </> "backend_known_hosts"
      sshOpts = sshArgs knownHostsPath (deployPath </> "ssh_key") False
  withSpinner "Uploading closures" $ ifor_ buildOutputByHost $ \host outputPath -> do
    callProcess'
      (Map.fromList [("NIX_SSHOPTS", unwords sshOpts)])
      "nix-copy-closure" ["-v", "--to", "--use-substitutes", "root@" <> host, "--gzip", outputPath]
  withSpinner "Uploading config" $ ifor_ buildOutputByHost $ \host _ -> do
    callProcessAndLogOutput (Notice, Warning) $
      proc rsyncPath
        [ "-e " <> sshPath <> " " <> unwords sshOpts
        , "-qarvz"
        , deployPath </> "config"
        , "root@" <> host <> ":/var/lib/backend"
        ]
  --TODO: Create GC root so we're sure our closure won't go away during this time period
  withSpinner "Switching to new configuration" $ ifor_ buildOutputByHost $ \host outputPath -> do
    callProcessAndLogOutput (Notice, Warning) $
      proc sshPath $ sshOpts <>
        [ "root@" <> host
        , unwords
            [ "bash -c"
            , bashEscape (deployActivationScript outputPath)
            ]
        ]
  isClean <- checkGitCleanStatus deployPath True
  when (not isClean) $ do
    withSpinner "Committing changes to Git" $ do
      callProcessAndLogOutput (Debug, Error) $
        gitProc deployPath ["add", "."]
      callProcessAndLogOutput (Debug, Error) $
        gitProc deployPath ["commit", "-m", "New deployment"]
  putLog Notice $ "Deployed => " <> T.pack route
  where
    callProcess' envMap cmd args = do
      let p = setEnvOverride (envMap <>) $ setDelegateCtlc True $ proc cmd args
      callProcessAndLogOutput (Notice, Notice) p

-- | Bash command that will be run on the deployed machine to actually switch the NixOS configuration
-- This has some more involved logic than merely activating the right profile. It also determines
-- whether the kernel parameters have changed so that the deployed NixOS instance should be restarted.
deployActivationScript
  :: String
  -- ^ The out path of the configuration to activate
  -> String
deployActivationScript outPath =
-- Note that we don't want to $(staticWhich "nix-env") here, because this is executing on a remote machine
-- This logic follows the nixos auto-upgrade module as of writing.
-- If the workflow is added to switch-to-configuration proper, we can simplify this:
-- https://github.com/obsidiansystems/obelisk/issues/958
  [i|set -euxo pipefail
nix-env -p /nix/var/nix/profiles/system --set "${bashEscape outPath}"
/nix/var/nix/profiles/system/bin/switch-to-configuration boot
booted="$(readlink /run/booted-system/{initrd,kernel,kernel-modules})"
built="$(readlink /nix/var/nix/profiles/system/{initrd,kernel,kernel-modules})"
/nix/var/nix/profiles/system/bin/switch-to-configuration switch
|]

-- | Update the source thunk in the staging directory to the HEAD of the branch.
deployUpdate :: MonadObelisk m => FilePath -> m ()
deployUpdate deployPath = wrapNixThunkError $
  updateThunkToLatest (ThunkUpdateConfig Nothing (ThunkConfig Nothing)) (deployPath </> "src")

-- | Platforms that we deploy obelisk artefacts to.
data PlatformDeployment = Android | IOS
  deriving (Show, Eq)

-- | Pretty print PlatformDeployment
renderPlatformDeployment :: PlatformDeployment -> String
renderPlatformDeployment = \case
  Android -> "android"
  IOS -> "ios"

-- | Produce the mobile app for an Obelisk project and deploy it onto a personal device.
-- This does not submit the artefacts to any app stores, or anything like that. It is
-- primarily useful for testing, or individual use of an Obelisk project.
deployMobile
  :: forall m. MonadObelisk m
  => PlatformDeployment
  -- ^ Which mobile artefact to deploy; e.g. Android or iOS
  -> [String]
  -- ^ Extra arguments to pass to the executable that actually loads
  -- the artefact onto the testing device. An example is the Team ID
  -- associated with an Apple developer account.
  -> m ()
deployMobile platform mobileArgs = withProjectRoot "." $ \root -> do
  let srcDir = root </> "src"
      configDir = root </> "config"
  exists <- liftIO $ doesDirectoryExist srcDir
  unless exists $ failWith "ob test should be run inside of a deploy directory"
  (nixBuildTarget, extraArgs) <- case platform of
    Android -> do
      let keystorePath = root </> "android_keystore.jks"
          keytoolConfPath = root </> "android_keytool_config.json"
      hasKeystore <- liftIO $ doesFileExist keystorePath
      when (not hasKeystore) $ do
        -- TODO log instructions for how to modify the keystore
        putLog Notice $ "Creating keystore: " <> T.pack keystorePath
        putLog Notice "Enter a keystore password: "
        keyStorePassword <- liftIO $ withEcho False getLine
        putLog Notice "Re-enter the keystore password: "
        keyStorePassword' <- liftIO $ withEcho False getLine
        unless (keyStorePassword' == keyStorePassword) $ failWith "passwords do not match"
        let keyToolConf = KeytoolConfig
              { _keytoolConfig_keystore = keystorePath
              , _keytoolConfig_alias = "obelisk"
              , _keytoolConfig_storepass = keyStorePassword
              , _keytoolConfig_keypass = keyStorePassword
              }
        createKeystore root keyToolConf
        liftIO $ BSL.writeFile keytoolConfPath $ encode keyToolConf
      checkKeytoolConfExist <- liftIO $ doesFileExist keytoolConfPath
      unless checkKeytoolConfExist $ failWith "Missing android KeytoolConfig"
      keytoolConfContents <- liftIO $ BSL.readFile keytoolConfPath
      keyArgs <- case eitherDecode keytoolConfContents :: Either String KeytoolConfig of
        Left err -> failWith $ T.pack err
        Right conf -> pure
          [ "--sign"
          , "--store-file", _keytoolConfig_keystore conf
          , "--store-password", _keytoolConfig_storepass conf
          , "--key-alias", _keytoolConfig_alias conf
          , "--key-password", _keytoolConfig_keypass conf
          ]
      let expr = mconcat
            [ "with (import ", toNixPath srcDir, " {});"
            , "android.frontend.override (drv: {"
            , "isRelease = true;"
            , "staticSrc = (passthru.__androidWithConfig ", configDir, ").frontend.staticSrc;"
            , "assets = (passthru.__androidWithConfig ", configDir, ").frontend.assets;"
            , "})"
            ]
      return (Target
        { _target_path = Nothing
        , _target_attr = Nothing
        , _target_expr = Just expr
        }, keyArgs)
    IOS -> do
      let expr = mconcat
            [ "with (import ", toNixPath srcDir, " {});"
            , "ios.frontend.override (_: { staticSrc = (passthru.__iosWithConfig ", toNixPath configDir, ").frontend.staticSrc; })"
            ]
      return (Target
        { _target_path = Nothing
        , _target_attr = Nothing
        , _target_expr = Just expr
        }, [])
  result <- nixCmd $ NixCmd_Build $ def
    & nixBuildConfig_outLink .~ OutLink_None
    & nixCmdConfig_target .~ nixBuildTarget
  let mobileArtifact = case platform of
                         IOS -> "iOS App"
                         Android -> "Android APK"
  putLog Notice $ T.pack $ unwords ["Your recently built", mobileArtifact, "can be found at the following path:", show result]
  callProcessAndLogOutput (Notice, Error) $ proc (result </> "bin" </> "deploy") (mobileArgs ++ extraArgs)
  where
    withEcho showEcho f = bracket
      (do
        prevEcho <- hGetEcho stdin
        hSetEcho stdin showEcho
        pure prevEcho
      )
      (hSetEcho stdin)
      (const f)

-- | obelisk uses keytool, a certificate and keypair management tool that comes with Java,
-- to manage the cryptographic assets needed to deploy to an Android device.
data KeytoolConfig = KeytoolConfig
  { _keytoolConfig_keystore :: FilePath
  -- ^ Where is the keystore that keytool should create keypairs?
  , _keytoolConfig_alias :: String
  -- ^ Name of the entry in the keystore to process
  , _keytoolConfig_storepass :: String
  -- ^ Password for the keystore
  , _keytoolConfig_keypass :: String
  -- ^ Password for the keypair under consideration
  } deriving (Show, Generic)

instance FromJSON KeytoolConfig
instance ToJSON KeytoolConfig

-- | Creates a keystore, and a keypair in that keystore.
createKeystore :: MonadObelisk m => FilePath -> KeytoolConfig -> m ()
createKeystore root config =
  callProcessAndLogOutput (Notice, Notice) $ setCwd (Just root) $ proc jreKeyToolPath
    [ "-genkeypair", "-noprompt"
    , "-keystore", _keytoolConfig_keystore config
    , "-keyalg", "RSA", "-keysize", "2048"
    , "-validity", "1000000"
    , "-storepass", _keytoolConfig_storepass config
    , "-alias", _keytoolConfig_alias config
    , "-keypass", _keytoolConfig_keypass config
    ]

-- | Simplified deployment configuration mechanism. At one point we may revisit this.
writeDeployConfig :: MonadObelisk m => FilePath -> FilePath -> String -> m ()
writeDeployConfig deployDir fname = liftIO . writeFile (deployDir </> fname)

-- | Read the deployment config file from a deployment staging directory.
readDeployConfig
  :: MonadObelisk m
  => FilePath
  -- ^ Deployment staging directory
  -> FilePath
  -- ^ The path to the config file relative to the staging directory.
  -> m String
readDeployConfig deployDir fname = liftIO $ do
  fmap (T.unpack . T.strip) $ T.readFile $ deployDir </> fname

-- | Lookup known hosts using ssh-keygen command
lookupKnownHosts :: MonadObelisk m
                 => String
                 -- ^ the host name
                 -> m [BS.ByteString]
                 -- ^ obtained hosts
lookupKnownHosts hostName =
  fmap filterComments $ readCreateProcessWithExitCode $ proc $(staticWhichNix "ssh-keygen") ["-F", hostName]
   where
     filterComments (exitCode, out, _) =
       if exitCode /= ExitSuccess || null out
         then []
         else
           -- ssh-keygen prints the following above each result it finds: "# Host <hostname> found: line <lineno>"
           filter (not . C.isPrefixOf "# Host") $ C.lines $ C.pack out

-- | insert a host/pair in backend_known_hosts file
addKnownHostFromEnv :: MonadObelisk m
                    => String
                    -- ^ hostname
                    -> FilePath
                    -- ^ path to backend_known_hosts file
                    -> m ()
addKnownHostFromEnv hostName obKnownHostsPath = do
  lookupKnownHosts hostName >>= \res -> case res of
    [knownKey] -> liftIO $ BS.appendFile obKnownHostsPath (knownKey `BS.append` C.singleton '\n')
    [] -> putLog Notice "Found no matching hosts in user's known_hosts file"
    _ -> putLog Notice "Found more than one matching host/key pair in user's known_hosts"

-- | Verify the identity of a remote host that we would like to deploy to.
verifyHostKey
  :: MonadObelisk m
  => FilePath
  -- ^ known_hosts file to use for hosts that have already been verified.
  -> FilePath
  -- ^ Path to the ssh key used to connect to the host
  -> String
  -- ^ Name of the host
  -> m ()
verifyHostKey knownHostsPath keyPath hostName =
  callProcessAndLogOutput (Notice, Warning) $ proc sshPath $
    sshArgs knownHostsPath keyPath True <>
      [ "root@" <> hostName
      , "-o", "NumberOfPasswordPrompts=0"
      , "exit"
      ]

-- | Create arguments to pass to ssh on the command line
sshArgs
  :: FilePath
  -- ^ Path to known_hosts file
  -> FilePath
  -- ^ Path to the ssh key to use
  -> Bool
  -- ^ If true, then prompt the user when a host is not in the known_hosts file,
  -- otherwise use strict host checking.
  -> [String]
sshArgs knownHostsPath keyPath askHostKeyCheck =
  [ "-o", "UserKnownHostsFile=" <> knownHostsPath
  , "-o", "StrictHostKeyChecking=" <> if askHostKeyCheck then "ask" else "yes"
  , "-i", keyPath
  ]

-- common/route validation
-- TODO: move these to executable-config once the typed-config stuff is done.

-- | Ways in which the route configured for a deployment host can be invalid
data InvalidRoute
  = InvalidRoute_NotHttps URI
  -- ^ We do not deploy non-https routes unless explicitly asked for
  | InvalidRoute_MissingScheme URI
  -- ^ We demand a URI scheme
  | InvalidRoute_MissingHost URI
  -- ^ We demand a hostname
  | InvalidRoute_HasPort URI
  -- ^ We do not deploy to a route with a particular port number
  | InvalidRoute_HasPath URI
  -- ^ We do not deploy to a route that is served at a particular path
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

-- | When deploying, we ensure that the route we are deploying for makes sense.
-- In particular, we extract the hostname that we are deploying to from the
-- route.
validateCommonRouteAndGetHost
  :: (MonadThrow m, MonadObelisk m)
  => Bool
  -- ^ If true, demand that the route we are deploying is an HTTPS route
  -> URI
  -- ^ The route to validate
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

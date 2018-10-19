{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Obelisk.Command.VmBuilder where

import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.Catch (try)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.String.Here.Uninterpolated (hereLit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath ((<.>), (</>))
import qualified System.Info
import System.Process (proc)

import Obelisk.App (MonadObelisk, getObeliskUserStateDir)
import Obelisk.CliApp
import Obelisk.Command.Nix (withNixRemoteCheck)

-- | Generate the `--builders` argument string to enable the VM builder after ensuring it is available.
getNixBuildersArg :: MonadObelisk m => m String
getNixBuildersArg = do
  stateDir <- liftIO getDockerBuilderStateDir
  exists <- containerExists
  if exists then startContainer else setupNixDocker stateDir
  pure $ nixBuildersArgString stateDir

-- | String to pass to nix's `--builders` arguments to enable the VM builder.
nixBuildersArgString :: FilePath -> String
nixBuildersArgString stateDir = unwords [containerName, "x86_64-linux", stateDir </> sshKeyFileName, "1", "1", "kvm"]

-- | Name of Docker container used for the VM builder.
containerName :: IsString str => str
containerName = "obelisk-docker-nix-builder"

-- | Check to see if the Docker container exists. This will exit with a helpful message if Docker is not installed.
containerExists :: MonadObelisk m => m Bool
containerExists = withExitFailMessage needDockerMsg $ do
  containerNames <- fmap (map T.strip . T.lines) $
    readProcessAndLogStderr Error $
      proc "docker" ["container", "list", "--all", "--format", "{{.Names}}"]
  pure $ containerName `elem` containerNames
  where
    needDockerMsg = "This feature requires that you have Docker installed and the `docker` command available on your PATH. Please go https://docs.docker.com/ to install Docker and try this command again."

-- | SSH port on localhost that connects to the container.
containerSshPort :: Int
containerSshPort = 2222

-- | Start the Docker container; assumes it already exists.
startContainer :: MonadObelisk m => m ()
startContainer = withSpinner "Starting VM builder" $
  callProcessAndLogOutput (Debug, Debug) $
    proc "docker" ["start", containerName]

-- | Creates the Docker container; assumes it does not exist.
setupNixDocker :: MonadObelisk m => FilePath -> m ()
setupNixDocker stateDir = withSpinner ("Creating Docker container named " <> containerName) $ do
  liftIO $ do
    createDirectoryIfMissing True stateDir
    T.writeFile (stateDir </> "Dockerfile") dockerfile

  -- Create new SSH keys for this container
  callProcessAndLogOutput (Debug, Error) $
    proc "rm" ["-f", stateDir </> sshKeyFileName, stateDir </> sshKeyFileName <.> "pub"]
  callProcessAndLogOutput (Debug, Error) $
    proc "ssh-keygen" ["-t", "ed25519", "-f", stateDir </> sshKeyFileName, "-P", ""]

  -- Build the docker container (which uses the SSH keys in the 'ssh' folder)
  containerId <- fmap T.strip $ readProcessAndLogStderr Error $
    proc "docker" ["build", stateDir, "--quiet"]
  callProcessAndLogOutput (Debug, Error) $ proc "docker"
    [ "run"
    , "--restart", "always"
    , "--detach"
    , "--publish", show containerSshPort <> ":22"
    , "--name", T.unpack containerName, T.unpack containerId
    ]
  exists <- containerExists
  unless exists $
    failWith $ "Expected to see docker container named " <> containerName <> " but it does not exist."

  linuxBuildWorked <- if System.Info.os == "linux"
    then pure False -- The Linux test is useless on Linux so just assume the setup is incomplete.
    else testLinuxBuild stateDir
  unless linuxBuildWorked $ do
    let sshIdFile = stateDir </> sshKeyFileName
    failWith $ setupInstructions sshIdFile

-- | The instructions for setting up SSH access to the container for the Nix daemon.
setupInstructions :: FilePath -> Text
setupInstructions sshIdFile = T.unlines
  [ "Setting Up Docker Nix Builder"
  , "-----------------------------"
  , ""
  , "We've created a Docker container that can build for Linux. However, the Nix"
  , "daemon needs to connect to this container as root. Please run the following"
  , "commands in a root shell (`sudo su -`) to tell SSH how to access the"
  , "container. Then try your obelisk command again."
  , ""
  , "# sudo su -"
  , "touch ~/.ssh/config"
  , "cat >> ~/.ssh/config <<CONF"
  , ""
  , sshConfigHost sshIdFile
  , "CONF"
  , "ssh " <> containerName <> " nix --version # Answer 'yes' if prompted"
  ]

-- | SSH configuration for `.ssh/config` to connect to the Docker container.
sshConfigHost :: FilePath -> Text
sshConfigHost sshIdFile = T.unlines
  [ "Host " <> containerName
  , "  User root"
  , "  HostName 127.0.0.1"
  , "  Port " <> T.pack (show containerSshPort)
  , "  IdentityFile " <> T.pack (show sshIdFile) -- TODO: Check to see how weird paths should be encoded here.
  ]

-- | User directory where state (namely the SSH keys) is kept for the Docker container.
getDockerBuilderStateDir :: IO FilePath
getDockerBuilderStateDir = liftA2 (</>) getObeliskUserStateDir (pure "nix-docker-builder")

-- | Run a test build to see if a Linux build can finish successfully.
testLinuxBuild :: MonadObelisk m => FilePath -> m Bool
testLinuxBuild stateDir =
  fmap (isRight :: Either ExitCode () -> Bool) $ try $ withNixRemoteCheck $
  callProcessAndLogOutput (Debug, Debug) $ proc "nix-build"
    [ "-E", "(import <nixpkgs> { system = \"x86_64-linux\"; }).writeText \"test\" builtins.currentTime"
    , "--builders", nixBuildersArgString stateDir
    ]

-- Copied from https://raw.githubusercontent.com/LnL7/nix-docker/bf28b99aebd8e403d2fc2171f4fa8878f857171c/ssh/Dockerfile
-- Renamed "insecure_rsa" to 'sshKeyFileName'
dockerfile :: Text
dockerfile = [hereLit|
FROM lnl7/nix:2018-04-17

RUN nix-env -f '<nixpkgs>' -iA \
    gnused \
    openssh \
 && nix-store --gc

RUN mkdir -p /etc/ssh \
 && echo "sshd:x:498:65534::/var/empty:/run/current-system/sw/bin/nologin" >> /etc/passwd \
 && cp /root/.nix-profile/etc/ssh/sshd_config /etc/ssh \
 && sed -i '/^PermitRootLogin/d' /etc/ssh/sshd_config \
 && echo "PermitRootLogin yes" >> /etc/ssh/sshd_config \
 && ssh-keygen -f /etc/ssh/ssh_host_rsa_key -N "" -t rsa \
 && ssh-keygen -f /etc/ssh/ssh_host_dsa_key -N "" -t dsa \
 && echo "export SSL_CERT_FILE=$SSL_CERT_FILE" >> /etc/bashrc \
 && echo "export PATH=$PATH" >> /etc/bashrc \
 && echo "export NIX_PATH=$NIX_PATH" >> /etc/bashrc \
 && echo "source /etc/bashrc" >> /etc/profile

|] <> T.unlines
  [ "ADD " <> T.pack sshKeyFileName <> " /root/.ssh/id_rsa"
  , "ADD " <> T.pack (sshKeyFileName <.> "pub") <> " /root/.ssh/authorized_keys"
  ] <> [hereLit|

EXPOSE 22
CMD ["/nix/store/hpnx760s247labqc3nbn2kripk73p0ca-openssh-7.6p1/bin/sshd", "-D", "-e"]
|]

sshKeyFileName :: FilePath
sshKeyFileName = "id_ed25519_obelisk_vm"

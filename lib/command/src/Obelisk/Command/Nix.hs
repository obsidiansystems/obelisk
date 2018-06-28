{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Nix
  ( nixBuild
  , withNixRemoteCheck
  , NixBuildConfig (..)
  , Target (..)
  , OutLink (..)
  , Arg (..)
  ) where

import Control.Monad.Catch (catch, throwM)
import Control.Monad.IO.Class (liftIO)
import Data.Default
import Data.List (intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Process (proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

-- | Where to put nix-build output
data OutLink
   = OutLink_Default
   | OutLink_None
   | OutLink_IndirectRoot FilePath

instance Default OutLink where
  def = OutLink_Default

-- | What to build
data Target = Target
  { _target_path :: FilePath
  , _target_attr :: Maybe String
  }

instance Default Target where
  def = Target
    { _target_path = "."
    , _target_attr = Nothing
    }

data Arg = Arg
  { _arg_key :: String
  , _arg_value :: String
  }

data NixBuildConfig = NixBuildConfig
  { _nixBuildConfig_target :: Target
  , _nixBuildConfig_outLink :: OutLink
  , _nixBuildConfig_args :: [Arg]
  , _nixBuildConfig_builders :: [String]
  }

instance Default NixBuildConfig where
  def = NixBuildConfig def def mempty mempty

nixBuild :: MonadObelisk m => NixBuildConfig -> m FilePath
nixBuild cfg = withSpinner' ("Running nix-build on " <> desc) (Just $ const $ "Built " <> desc) $ do
  withNixRemoteCheck $ readProcessAndLogStderr Debug $ proc "nix-build" $ mconcat
    [[path], attrArg, args, outLink, buildersArg]
  where
    path = _target_path $ _nixBuildConfig_target cfg
    attr = _target_attr $ _nixBuildConfig_target cfg
    attrArg = case attr of
      Nothing -> []
      Just a -> ["-A", a]
    args = mconcat [["--argstr", k, v] | Arg k v <- _nixBuildConfig_args cfg]
    outLink = case _nixBuildConfig_outLink cfg of
      OutLink_Default -> []
      OutLink_None -> ["--no-out-link"]
      OutLink_IndirectRoot l -> ["--out-link", l]
    desc = T.pack $ path <> maybe "" (\a -> " [" <> a <> "]") attr
    buildersArg = case _nixBuildConfig_builders cfg of
      [] -> []
      builders -> ["--builders", intercalate ";" builders]

-- | If a nix command fails, and this may be related to the NIX_REMOTE issue,
-- tell the user what to do.
--
-- Added: June, 2018. Consider removing this eventually.
withNixRemoteCheck :: MonadObelisk m => m a -> m a
withNixRemoteCheck f = f `catch` \e@(ProcessFailed _ _) -> do
  liftIO (lookupEnv "NIX_REMOTE") >>= \case
    Just _ -> throwM e
    Nothing -> liftIO (writable <$> getPermissions "/nix/var/nix/db") >>= \case
      True -> throwM e
      False -> do
        putLog Error "!!! "
        putLog Error "!!! A nix command failed to run. You might need to set the NIX_REMOTE environment variable"
        putLog Error "!!! to `daemon`. To do this, run the following before running obelisk:"
        putLog Error "!!! "
        putLog Error "!!!     export NIX_REMOTE=daemon"
        putLog Error "!!! "
        putLog Error "!!! For details, see https://github.com/NixOS/nixpkgs/issues/5713"
        putLog Error "!!! "
  throwM e

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Nix
  ( nixBuild
  , NixBuildConfig (..)
  , Target (..)
  , OutLink (..)
  , Arg (..)
  ) where

import Data.Default
import Data.List (intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as T
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
nixBuild cfg = withSpinner' ("Running nix-build on " <> desc, const $ Just $ "Built " <> desc) $ do
  readProcessAndLogStderr Debug $ proc "nix-build" $ mconcat
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

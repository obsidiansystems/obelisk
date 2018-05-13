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
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Process (proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CLI (Severity (..), readProcessAndLogStderr, withSpinner)

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
  }

instance Default NixBuildConfig where
  def = NixBuildConfig def def mempty

nixBuild :: MonadObelisk m => NixBuildConfig -> m FilePath
nixBuild cfg = do
  let args = mconcat
        [ [_target_path $ _nixBuildConfig_target cfg]
        , case _target_attr $ _nixBuildConfig_target cfg of
            Nothing -> []
            Just attr -> ["-A", attr]
        , mconcat [["--argstr", k, v] | Arg k v <- _nixBuildConfig_args cfg]
        , case _nixBuildConfig_outLink cfg of
            OutLink_Default -> []
            OutLink_None -> ["--no-out-link"]
            OutLink_IndirectRoot outLink -> ["--out-link", outLink]
        ]
  let msg = T.pack $ "Running nix-build (" <> _target_path (_nixBuildConfig_target cfg) <> ")"
  withSpinner msg $ do
    readProcessAndLogStderr Debug (proc "nix-build" args)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.Command.Nix
  ( nixBuild
  , NixBuildConfig (..)
  , Target (..)
  , OutLink (..)
  , Arg (..)
  , boolArg
  , strArg
  ) where

import Data.Bool (bool)
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

data Arg
  = Arg_Str String String
  | Arg_Expr String String
  deriving (Eq, Show)

strArg :: String -> String -> Arg
strArg k = Arg_Str k

boolArg :: String -> Bool -> Arg
boolArg k = Arg_Expr k . bool "false" "true"

cliFromArgs :: [Arg] -> [String]
cliFromArgs = concatMap $ \case
  Arg_Str k v -> ["--argstr", k, v]
  Arg_Expr k v -> ["--arg", k, v]

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
  readProcessAndLogStderr Debug $ proc "nix-build" $ mconcat
    [[path], attrArg, args, outLink, buildersArg]
  where
    path = _target_path $ _nixBuildConfig_target cfg
    attr = _target_attr $ _nixBuildConfig_target cfg
    attrArg = case attr of
      Nothing -> []
      Just a -> ["-A", a]
    args = cliFromArgs $ _nixBuildConfig_args cfg
    outLink = case _nixBuildConfig_outLink cfg of
      OutLink_Default -> []
      OutLink_None -> ["--no-out-link"]
      OutLink_IndirectRoot l -> ["--out-link", l]
    desc = T.pack $ path <> maybe "" (\a -> " [" <> a <> "]") attr
    buildersArg = case _nixBuildConfig_builders cfg of
      [] -> []
      builders -> ["--builders", intercalate ";" builders]

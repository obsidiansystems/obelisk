{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command.Nix
  ( nixCmd
  , NixCmd (..)
  , nixCmdConfig_target
  , nixCmdConfig_args
  , nixCmdConfig_builders
  , NixBuildConfig (..)
  , nixBuildConfig_outLink
  , NixInstantiateConfig (..)
  , nixInstantiateConfig_eval
  , NixCommonConfig (..)
  , Target (..)
  , OutLink (..)
  , Arg (..)
  , boolArg
  , rawArg
  , strArg
  ) where

import Control.Monad (guard)
import Control.Lens

import Data.Bool (bool)
import Data.Default
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Process (proc)

import Obelisk.App (MonadObelisk)
import Obelisk.CliApp

-- | What to build
data Target = Target
  { _target_path :: Maybe FilePath
  , _target_attr :: Maybe String
  , _target_expr :: Maybe String
  }
makeClassy ''Target

instance Default Target where
  def = Target
    { _target_path = Just "."
    , _target_attr = Nothing
    , _target_expr = Nothing
    }

data Arg
  = Arg_Str String String
  | Arg_Expr String String
  deriving (Eq, Show)

strArg :: String -> String -> Arg
strArg k = Arg_Str k

rawArg :: String -> String -> Arg
rawArg k = Arg_Expr k

boolArg :: String -> Bool -> Arg
boolArg k = Arg_Expr k . bool "false" "true"

cliFromArgs :: [Arg] -> [String]
cliFromArgs = concatMap $ \case
  Arg_Str k v -> ["--argstr", k, v]
  Arg_Expr k v -> ["--arg", k, v]

data NixCommonConfig = NixCommonConfig
  { _nixCmdConfig_target :: Target
  , _nixCmdConfig_args :: [Arg]
  , _nixCmdConfig_builders :: [String]
  }
makeClassy ''NixCommonConfig

instance Default NixCommonConfig where
  def = NixCommonConfig def mempty mempty

runNixCommonConfig :: NixCommonConfig -> [FilePath]
runNixCommonConfig cfg = mconcat [maybeToList path, attrArg, exprArg, args, buildersArg]
  where
    path = _target_path $ _nixCmdConfig_target cfg
    attr = _target_attr $ _nixCmdConfig_target cfg
    expr = _target_expr $ _nixCmdConfig_target cfg
    attrArg = case attr of
      Nothing -> []
      Just a -> ["-A", a]
    exprArg = case expr of
      Nothing -> []
      Just a -> ["-E", a]
    args = cliFromArgs $ _nixCmdConfig_args cfg
    buildersArg = case _nixCmdConfig_builders cfg of
      [] -> []
      builders -> ["--builders", intercalate ";" builders]

-- | Where to put nix-build output
data OutLink
  = OutLink_Default
  | OutLink_None
  | OutLink_IndirectRoot FilePath

instance Default OutLink where
  def = OutLink_Default

data NixBuildConfig = NixBuildConfig
  { _nixBuildConfig_common :: NixCommonConfig
  , _nixBuildConfig_outLink :: OutLink
  }
makeLenses ''NixBuildConfig

instance HasNixCommonConfig NixBuildConfig where
  nixCommonConfig = nixBuildConfig_common

instance Default NixBuildConfig where
  def = NixBuildConfig def def

runNixBuildConfig :: NixBuildConfig -> [FilePath]
runNixBuildConfig cfg = mconcat
  [ runNixCommonConfig $ cfg ^. nixCommonConfig
  , case _nixBuildConfig_outLink cfg of
      OutLink_Default -> []
      OutLink_None -> ["--no-out-link"]
      OutLink_IndirectRoot l -> ["--out-link", l]
  ]

data NixInstantiateConfig = NixInstantiateConfig
  { _nixInstantiateConfig_common :: NixCommonConfig
  , _nixInstantiateConfig_eval :: Bool
  }
makeLenses ''NixInstantiateConfig

instance HasNixCommonConfig NixInstantiateConfig where
  nixCommonConfig = nixInstantiateConfig_common

instance Default NixInstantiateConfig where
  def = NixInstantiateConfig def False

runNixInstantiateConfig :: NixInstantiateConfig -> [FilePath]
runNixInstantiateConfig cfg = mconcat
  [ runNixCommonConfig $ cfg ^. nixCommonConfig
  , "--eval" <$ guard (_nixInstantiateConfig_eval cfg)
  ]

data NixCmd
  = NixCmd_Build NixBuildConfig
  | NixCmd_Instantiate NixInstantiateConfig

instance Default NixCmd where
  def = NixCmd_Build def

nixCmd :: MonadObelisk m => NixCmd -> m FilePath
nixCmd cmdCfg = withSpinner' ("Running " <> cmd <> desc) (Just $ const $ "Built " <> desc) $ do
  output <- readProcessAndLogStderr Debug $ proc (T.unpack cmd) $ options
  -- Remove final newline that Nix appends
  Just (outPath, '\n') <- pure $ T.unsnoc output
  pure $ T.unpack outPath
  where
    (cmd, options, commonCfg) = case cmdCfg of
      NixCmd_Build cfg' ->
        ( "nix-build"
        , runNixBuildConfig cfg'
        , cfg' ^. nixCommonConfig
        )
      NixCmd_Instantiate cfg' ->
        ( "nix-instantiate"
        , runNixInstantiateConfig cfg'
        , cfg' ^. nixCommonConfig
        )
    path = commonCfg ^. nixCmdConfig_target . target_path
    desc = T.pack $ mconcat $ catMaybes
      [ (" on " <>) <$> path
      , (\a -> " [" <> a <> "]") <$> (commonCfg ^. nixCmdConfig_target . target_attr)
      ]


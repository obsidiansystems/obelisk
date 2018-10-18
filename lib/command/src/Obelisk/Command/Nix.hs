{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command.Nix
  ( nixBuild
  , withNixRemoteCheck
  , NixBuildConfig (..)
  , Target (..)
  , OutLink (..)
  , Arg (..)
  , boolArg
  , rawArg
  , strArg
  ) where

--import Control.Monad.Catch (catch, throwM)
import Control.Lens
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Bool (bool)
import Data.Default
import Data.List (intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as T
import System.Directory
import System.Environment
import System.Process (proc)

import Obelisk.App (MonadInfallibleObelisk, MonadObelisk, ObeliskError (..))
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

rawArg :: String -> String -> Arg
rawArg k = Arg_Expr k

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
  withNixRemoteCheck $ fmap T.unpack $ readProcessAndLogStderr Debug $ proc "nix-build" $ mconcat
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

-- | If a nix command fails, and this may be related to the NIX_REMOTE issue,
-- tell the user what to do.
--
-- Added: June, 2018. Consider removing this eventually.
withNixRemoteCheck
  :: MonadObelisk m
  => (forall m'. MonadInfallibleObelisk m' => ExceptT ObeliskError m' a)
  -> m a
withNixRemoteCheck f = do
  status <- runExceptT f
  case status of
    Right a -> pure a
    Left e -> throwError =<< case matching asProcessFailure e of
      Left _ -> pure e
      Right pf -> liftIO (lookupEnv "NIX_REMOTE") >>= \case
        Just _ -> pure e
        Nothing -> liftIO (writable <$> getPermissions "/nix/var/nix/db") >>= \case
          True -> pure e
          False -> pure $ ObeliskError_ProcessError pf $ Just $ T.unlines
            [ "!!! "
            , "!!! A nix command failed to run. You might need to set the NIX_REMOTE environment variable"
            , "!!! to `daemon`. To do this, run the following before running obelisk:"
            , "!!! "
            , "!!!     export NIX_REMOTE=daemon"
            , "!!! "
            , "!!! For details, see https://github.com/NixOS/nixpkgs/issues/5713"
            , "!!! "
            ]

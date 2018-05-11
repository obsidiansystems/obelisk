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
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as LBS
import Data.Default
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit (ExitCode (ExitSuccess))
import System.Process (StdStream (CreatePipe), createProcess, proc, std_err, std_out, waitForProcess)

import Obelisk.App (MonadObelisk)
import Obelisk.Command.CLI (failWith, withSpinner)

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
  (_, Just out, Just err, p) <- liftIO $ createProcess (proc "nix-build" args)
    { std_out = CreatePipe
    , std_err = CreatePipe
    }
  let msg = T.pack $ "Running nix-build [" <> _target_path (_nixBuildConfig_target cfg) <> "] ..."
  withSpinner msg Nothing $ do
    liftIO $ waitForProcess p >>= \case
      ExitSuccess -> return ()
      _ -> do
        -- FIXME: We should interleave `out` and `err` in their original order?
        LBS.putStr =<< LBS.hGetContents out
        LBS.putStr =<< LBS.hGetContents err
        failWith "nix-build failed"
  liftIO $ T.unpack . T.strip <$> T.hGetContents out

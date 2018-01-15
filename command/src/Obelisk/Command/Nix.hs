{-# LANGUAGE LambdaCase #-}
module Obelisk.Command.Nix
  ( nixBuild
  , NixBuildConfig (..)
  , Target (..)
  , OutLink (..)
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Exit
import System.Process

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

data NixBuildConfig = NixBuildConfig
  { _nixBuildConfig_target :: Target
  , _nixBuildConfig_outLink :: OutLink
  }

instance Default NixBuildConfig where
  def = NixBuildConfig def def

nixBuild :: NixBuildConfig -> IO FilePath
nixBuild cfg = do
  let args = mconcat
        [ [_target_path $ _nixBuildConfig_target cfg]
        , case _target_attr $ _nixBuildConfig_target cfg of
            Nothing -> []
            Just attr -> ["-A", attr]
        , case _nixBuildConfig_outLink cfg of
            OutLink_Default -> []
            OutLink_None -> ["--no-out-link"]
            OutLink_IndirectRoot outLink -> ["--indirect", "--add-root", outLink]
        ]
  (_, out, err, p) <- runInteractiveProcess "nix-build" args Nothing Nothing
  waitForProcess p >>= \case
    ExitSuccess -> return ()
    _ -> do
      LBS.putStr =<< LBS.hGetContents out
      LBS.putStr =<< LBS.hGetContents err
      fail "nix-build failed"
  T.unpack . T.strip <$> T.hGetContents out

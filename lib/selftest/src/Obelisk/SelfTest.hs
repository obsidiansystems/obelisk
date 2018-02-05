{-# LANGUAGE OverloadedStrings #-}
module Obelisk.SelfTest where

import Control.Monad
import Data.String
import qualified Data.Text as T
import Shelly
import System.Environment
import System.IO.Temp
import Test.Hspec

main :: IO ()
main = do
  obeliskImpl <- getEnv "OBELISK_IMPL"
  withSystemTempDirectory "blank-project" $ \blankProject ->
    hspec $ do
      describe "blank initialized project" $ do
        let inProj :: Sh a -> IO ()
            inProj = void . shelly . silently . chdir (fromString blankProject)
        it "can be created" $ inProj $ do
          run "ob" ["init", "--symlink", T.pack obeliskImpl]
        it "can build ghc.backend" $ inProj $ do
          run "nix-build" ["--no-out-link", "-A", "ghc.backend"]
        it "can build ghcjs.frontend" $ inProj $ do
          run "nix-build" ["--no-out-link", "-A", "ghcjs.frontend"]

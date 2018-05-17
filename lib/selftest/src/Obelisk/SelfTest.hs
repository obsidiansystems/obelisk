{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.SelfTest where

import Control.Exception (throw)
import Control.Monad
import Data.Function (fix)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Shelly
import System.Environment
import System.Exit (ExitCode(..))
import System.IO.Temp
import Test.Hspec

data ObRunState
  = ObRunState_Init
  | ObRunState_Running
  deriving (Eq, Show)

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
        describe "ob run" $ do
          it "can start frontend" $ inProj $ handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
            runHandle "ob" ["run"] $ \stdout -> flip fix ObRunState_Init $ \loop last -> do
              liftIO (T.hGetLine stdout) >>= \t -> case last of
                ObRunState_Init
                  | "Running test..." `T.isPrefixOf` t -> loop ObRunState_Running
                ObRunState_Running
                  | "Frontend running" `T.isPrefixOf` t -> exit 0 -- Frontend successfully started
                  | not (T.null t) -> errorExit t -- If theres any other output here, startup failed
                _ -> loop last

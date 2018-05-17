{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.SelfTest where

import Control.Exception (throw)
import Control.Monad
import Data.Function (fix)
import Data.Semigroup ((<>))
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Shelly
import System.Environment
import System.Exit (ExitCode(..))
import System.IO.Temp
import System.Timeout
import Test.Hspec

data ObRunState
  = ObRunState_Init
  | ObRunState_Startup
  | ObRunState_BackendStarted Text -- Port of backend
  | ObRunState_FrontendStarted Text Text -- Port of backend, URI of frontend
  deriving (Eq, Show)

main :: IO ()
main = do
  obeliskImpl <- getEnv "OBELISK_IMPL"
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
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

        it "can use ob run" $ inProj $ handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
          runHandle "ob" ["run"] $ \stdout -> flip fix ObRunState_Init $ \loop -> \case
            ObRunState_FrontendStarted backendPort frontendUri -> do
              let req uri = liftIO $ HTTP.parseRequest (T.unpack uri) >>= flip HTTP.httpLbs httpManager
              req frontendUri >>= \r -> when (HTTP.responseStatus r /= HTTP.ok200) $ errorExit $
                "Request to frontend server failed: " <> T.pack (show r)
              void $ req $ "http://localhost:" <> backendPort
              liftIO (timeout 1000000 $ T.hGetLine stdout) >>= \case
                Just t | "127.0.0.1" `T.isPrefixOf` t -> exit 0 -- Backend request successfully logged
                x -> errorExit $ "Couldn't get backend request log: " <> T.pack (show x)
            last -> liftIO (T.hGetLine stdout) >>= \t -> case last of
              ObRunState_Init
                | "Running test..." `T.isPrefixOf` t -> loop ObRunState_Startup
              ObRunState_Startup
                | Just port <- "Backend running on port " `T.stripPrefix` t
                -> loop $ ObRunState_BackendStarted port
                | not (T.null t) -> errorExit t -- If theres any other output here, startup failed
              ObRunState_BackendStarted port
                | Just uri <- "Frontend running on " `T.stripPrefix` t
                -> loop $ ObRunState_FrontendStarted port uri
                | not (T.null t) -> errorExit t -- If theres any other output here, startup failed
              _ -> loop last

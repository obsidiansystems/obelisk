{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Obelisk.SelfTest where

import Control.Monad.IO.Class
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
import System.IO (Handle)
import System.IO.Temp
import System.Timeout
import Test.Hspec

data ObRunState
  = ObRunState_Init
  | ObRunState_Startup
  | ObRunState_BackendStarted Text -- Port of backend
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

        forM_ ["ghc", "ghcjs"] $ \compiler -> do
          let
            shell = "shells." <> compiler
            inShell cmd = run "nix-shell" $ ["-A", fromString shell, "--run", cmd]
          it ("can enter "    <> shell) $ inProj $ inShell "exit"
          it ("can build in " <> shell) $ inProj $ inShell $ "cabal new-build --" <> fromString compiler <> " all"

        it "can use ob run" $ inProj $ handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
          runHandle "ob" ["run"] $ \stdout -> do
            (_, firstUri) <- obRun httpManager stdout
            let newUri = "http://localhost:8080/" -- trailing slash required for comparison
            when (firstUri == newUri) $ errorExit $
              "Default URI (" <> firstUri <> ") is the same as test URI (" <> newUri <> ")"
            alterRouteTo newUri stdout
            (_, runningUri) <- obRun httpManager stdout
            if runningUri /= newUri
            then errorExit $ "Reloading failed: expected " <> newUri <> " but got " <> runningUri
            else exit 0

-- | Blocks until a non-empty line is available
hGetLineSkipBlanks :: MonadIO m => Handle -> m Text
hGetLineSkipBlanks h = liftIO $ fix $ \loop -> T.hGetLine h >>= \case
  "" -> loop
  t -> pure t

-- | Alters the route file and waits for `ob run` to reload
alterRouteTo :: Text -> Handle -> Sh ()
alterRouteTo uri stdout = do
  writefile "config/common/route" uri
  hGetLineSkipBlanks stdout >>= \t -> when (t /= "Reloading...") $ errorExit $
    "Reloading failed: " <> T.pack (show t)
  hGetLineSkipBlanks stdout >>= \t -> when (t /= "  config/common/route") $ errorExit $
    "Reloading failed: " <> T.pack (show t)
  hGetLineSkipBlanks stdout >>= \t -> when (t /= "Interrupted.") $ errorExit $
    "Reloading failed: " <> T.pack (show t)

-- | Handle stdout of `ob run`: check that the frontend and backend servers are started correctly
obRun :: HTTP.Manager -> Handle -> Sh (Text, Text)
obRun httpManager stdout = flip fix ObRunState_Init $ \loop state -> do
  liftIO (T.hGetLine stdout) >>= \t -> case state of
    ObRunState_Init
      | "Running test..." `T.isPrefixOf` t -> loop ObRunState_Startup
    ObRunState_Startup
      | t == "backend stopped; make a change to your code to reload" -> loop ObRunState_Startup
      | Just port <- "Backend running on port " `T.stripPrefix` t -> loop $ ObRunState_BackendStarted port
      | not (T.null t) -> errorExit $ "Startup: " <> t -- If theres any other output here, startup failed
    ObRunState_BackendStarted port
      | Just uri <- "Frontend running on " `T.stripPrefix` t -> do
        obRunCheck httpManager stdout port uri
        pure (port, uri)
      | not (T.null t) -> errorExit $ "Started: " <> t -- If theres any other output here, startup failed
    _ -> loop state

-- | Make requests to frontend/backend servers to check they are working properly
obRunCheck :: HTTP.Manager -> Handle -> Text -> Text -> Sh ()
obRunCheck httpManager stdout backendPort frontendUri = do
  let req uri = liftIO $ HTTP.parseRequest (T.unpack uri) >>= flip HTTP.httpLbs httpManager
  req frontendUri >>= \r -> when (HTTP.responseStatus r /= HTTP.ok200) $ errorExit $
    "Request to frontend server failed: " <> T.pack (show r)
  void $ req $ "http://localhost:" <> backendPort
  liftIO (timeout 1000000 $ T.hGetLine stdout) >>= \case
    Just t | "127.0.0.1" `T.isPrefixOf` t -> pure () -- Backend request logged
    x -> errorExit $ "Couldn't get backend request log: " <> T.pack (show x)

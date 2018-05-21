{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.SelfTest where

import Control.Exception (Exception, throw)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function (fix)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import Shelly
import System.Environment
import System.Exit (ExitCode (..))
import System.Info
import System.IO (Handle, hClose)
import System.IO.Temp
import System.Timeout
import Test.Hspec
import Test.HUnit.Base
import System.Process (readProcessWithExitCode)

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
            thunk  = ".obelisk/impl"

            -- See https://github.com/obsidiansystems/obelisk/pull/92#issuecomment-390226735
            hashDir dir   = silently $ run "sh" ["-c", "cd " <> dir <> " && find . | grep -v '^./.git' | grep -v '/.attr-cache' | grep -v '^.$' | xargs -n 1 -I % -- sh -c 'echo `nix-hash %` %'"]
            hash = hashDir thunk

        it "can be created" $ inProj $ do
          run "ob" ["init"]

        it "can build ghc.backend" $ inProj $ do
          run "nix-build" ["--no-out-link", "-A", "ghc.backend"]
        it "can build ghcjs.frontend" $ inProj $ do
          run "nix-build" ["--no-out-link", "-A", "ghcjs.frontend"]

        if os == "darwin"
          then it "can build ios"     $ inProj $ run "nix-build" ["--no-out-link", "-A", "ios.frontend"    ]
          else it "can build android" $ inProj $ run "nix-build" ["--no-out-link", "-A", "android.frontend"]

        forM_ ["ghc", "ghcjs"] $ \compiler -> do
          let
            shell = "shells." <> compiler
            inShell cmd = run "nix-shell" ["-A", fromString shell, "--run", cmd]
          it ("can enter "    <> shell) $ inProj $ inShell "exit"
          it ("can build in " <> shell) $ inProj $ inShell $ "cabal new-build --" <> fromString compiler <> " all"

        it "has idempotent thunk update" $ inProj $ do
          let update = run "ob" ["thunk", "update", thunk] *> hash
          u  <- update
          uu <- update
          liftIO $ assertEqual "" u uu

        describe "ob thunk pack/unpack" $ do
          let pack   = run "ob" ["thunk", "pack",   thunk] *> hash
              unpack = run "ob" ["thunk", "unpack", thunk] *> hash

          it "has thunk pack and unpack inverses" $ inProj $ do
            e    <- hash
            eu   <- unpack
            eup  <- pack
            eupu <- unpack
            _    <- pack
            liftIO $ do
              assertEqual "" e  eup
              assertEqual "" eu eupu
              assertBool  "" (e /= eu)

          it "can pack and unpack plain git repos" $ do
            withSystemTempDirectory "git-repo" $ \dir -> shelly @IO $ silently $ do
              let repo = toTextIgnore $ dir </> ("repo" :: String)
              origHash <- run_ "git" ["clone", "https://git.haskell.org/packages/primitive.git", repo] *> hashDir repo

              run_ "ob" ["thunk", "pack", repo]
              packedFiles <- Set.fromList <$> ls (fromText repo)
              liftIO $ assertEqual "" packedFiles $
                Set.fromList $ (repo </>) <$> ["default.nix", "git.json", ".attr-cache" :: String]

              unpackHash <- run_ "ob" ["thunk", "unpack", repo] *> hashDir repo
              liftIO $ assertEqual "" origHash unpackHash

              testThunkPack $ fromText repo

          it "aborts thunk pack when there are uncommitted files" $ inProj $ do
            unpack
            testThunkPack (blankProject </> thunk)

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

testThunkPack :: Shelly.FilePath -> Sh ()
testThunkPack path = withTempFile (T.unpack $ toTextIgnore path) "test-file" $ \file handle -> do
  let try_sh :: Exception e => Sh a -> Sh (Either e a)
      try_sh a = catch_sh (a >>= pure . Right) (pure . Left)
      pack' = readProcessWithExitCode "ob" ["thunk", "pack", T.unpack $ toTextIgnore path] ""
      ensureThunkPackFails err = liftIO $ pack' >>= \case
        (code, out, _err)
          | code == ExitSuccess -> fail "ob thunk pack succeeded when it should have failed"
          | err `T.isInfixOf` T.pack out -> pure ()
          | otherwise -> fail $ "ob thunk pack failed for an unexpected reason: " <> show out
      git = chdir path . run "git"
  -- Untracked files
  ensureThunkPackFails "unsaved modifications"
  git ["add", T.pack file]
  -- Uncommitted files (staged)
  ensureThunkPackFails "unsaved modifications"
  git ["commit", "-m", "test commit"]
  -- Non-pushed commits in any branch
  ensureThunkPackFails "not been pushed"
  -- Uncommitted files (unstaged)
  liftIO $ T.hPutStrLn handle "test file" >> hClose handle
  ensureThunkPackFails "unsaved modifications"
  -- Existing stashes
  git ["stash"]
  ensureThunkPackFails "has stashes"

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

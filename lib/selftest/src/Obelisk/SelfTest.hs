{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.SelfTest where

import Control.Exception (bracket, throw)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Function (fix)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import Data.Void
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import Shelly
import System.Directory (withCurrentDirectory)
import System.Environment
import System.Exit (ExitCode (..))
import System.Info
import System.IO (Handle, hClose)
import System.IO.Temp
import System.Process (readProcessWithExitCode, CreateProcess(cwd), readCreateProcessWithExitCode, proc)
import Test.Hspec
import Test.HUnit.Base

import Obelisk.CliApp hiding (runCli, readCreateProcessWithExitCode)
import qualified Obelisk.CliApp as CliApp
import Obelisk.Run (getConfigRoute)

data ObRunState
  = ObRunState_Init
  | ObRunState_Startup
  | ObRunState_BackendStarted
  deriving (Eq, Show)

doubleQuotes :: (IsString a, Semigroup a) => a -> a
doubleQuotes s = "\"" <> s <> "\""

commit :: Text -> Sh ()
commit msg = void $ run "git"
  [ "commit"
  , "--no-gpg-sign"
  , "--allow-empty"
  , "-m"
  , doubleQuotes msg
  ]

-- TODO replace Void and stop using loosely typed synchronous exceptions
runCli :: MonadIO m => CliT Void IO a -> m a
runCli f = liftIO $ do
  c <- newCliConfig Notice False False (\case {})
  CliApp.runCli c f

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | Like `shelly` but used when running `ob` commands
--
-- This tests the NIX_REMOTE workaround, whether that gets printed
-- on failure, and whether retrying with the suggested workaround
-- leads to the test succeeding.
shellyOb :: MonadIO m => (Sh a -> Sh a) -> Sh a -> m a
shellyOb f obTest = shelly $ f $ do
  obTest `catch_sh` \(e :: RunFailed) -> do
    obStderr <- lastStderr
    if T.isInfixOf "export NIX_REMOTE" obStderr
      then do
      runCli $ do
        putLog Notice "Detected NIX_REMOTE suggestion in stderr of failed ob"
        putLog Warning "Retrying this test with NIX_REMOTE=daemon"
      setenv "NIX_REMOTE" "daemon"
      f obTest
      else throw e

main :: IO ()
main = do
  -- Note: you can pass hspec arguments as well, eg: `-m <pattern>`
  isVerbose <- (elem "-v") <$> getArgs
  unless isVerbose $
    putStrLn "Tests may take longer to run if there are unbuilt derivations: use -v for verbose output"
  let verbosity = bool silently verbosely isVerbose
  obeliskImpl <- fromString <$> getEnv "OBELISK_IMPL"
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  [p0, p1, p2, p3] <- liftIO $ getFreePorts 4
  withSystemTempDirectory "initCache" $ \initCache -> do
    -- Setup the ob init cache
    void . shellyOb verbosity $ chdir (fromString initCache) $ do
      run_ "ob" ["init"]
      run_ "git" ["init"]
    hspec $ parallel $ do
      let shelly_ = void . shellyOb verbosity

          inTmp :: (Shelly.FilePath -> Sh a) -> IO ()
          inTmp f = shelly_ . withSystemTempDirectory "test" $ (chdir <*> f) . fromString

          inTmpObInit f = inTmp $ \dir -> do
            run_ "cp" ["-a", fromString $ initCache <> "/.", toTextIgnore dir]
            f dir

          assertRevEQ a b = liftIO . assertEqual "" ""        =<< diff a b
          assertRevNE a b = liftIO . assertBool  "" . (/= "") =<< diff a b

          revParseHead = T.strip <$> run "git" ["rev-parse", "HEAD"]

          commitAll = do
            run_ "git" ["add", "."]
            commit "checkpoint"
            revParseHead

          thunk  = ".obelisk/impl"
          update = run "ob" ["thunk", "update", thunk] >> commitAll
          pack   = run "ob" ["thunk", "pack",   thunk] >> commitAll
          unpack = run "ob" ["thunk", "unpack", thunk] >> commitAll

          diff a b = run "git" ["diff", a, b]

      describe "ob init" $ parallel $ do
        it "works with default impl"       $ inTmp $ \_ -> run "ob" ["init"]
        it "works with master branch impl" $ inTmp $ \_ -> run "ob" ["init", "--branch", "master"]
        it "works with symlink"            $ inTmp $ \_ -> run "ob" ["init", "--symlink", obeliskImpl]
        it "doesn't silently overwrite existing files" $ withSystemTempDirectory "ob-init" $ \dir -> do
          let p force = (proc "ob" $ "--no-handoff" : "init" : if force then ["--force"] else []) { cwd = Just dir }
          (ExitSuccess, _, _) <- readCreateProcessWithExitCode (p False) ""
          (ExitFailure _, _, _) <- readCreateProcessWithExitCode (p False) ""
          (ExitSuccess, _, _) <- readCreateProcessWithExitCode (p True) ""
          pure ()

        it "doesn't create anything when given an invalid impl" $ inTmp $ \tmp -> do
          void $ errExit False $ run "ob" ["init", "--symlink", "/dev/null"]
          ls tmp >>= liftIO . assertEqual "" []

        it "produces a valid route config" $ inTmpObInit $ \tmp -> do
          liftIO $ withCurrentDirectory (T.unpack $ toTextIgnore tmp) $ getConfigRoute `shouldNotReturn` Nothing

      -- These tests fail with "Could not find module 'Obelisk.Generated.Static'"
      -- when not run by 'nix-build --attr selftest'
      describe "ob run" $ parallel $ do
        it "works in root directory" $ inTmpObInit $ \_ -> do
          testObRunInDir p0 p1 Nothing httpManager
        it "works in sub directory" $ inTmpObInit $ \_ -> do
          testObRunInDir p2 p3 (Just "frontend") httpManager

      describe "obelisk project" $ parallel $ do
        it "can build obelisk command"  $ shelly_ $ run "nix-build" ["-A", "command" , obeliskImpl]
        it "can build obelisk skeleton" $ shelly_ $ run "nix-build" ["-A", "skeleton", obeliskImpl]
        it "can build obelisk shell"    $ shelly_ $ run "nix-build" ["-A", "shell",    obeliskImpl]
        -- See https://github.com/obsidiansystems/obelisk/issues/101
        -- it "can build everything"       $ shelly_ $ run "nix-build" [obeliskImpl]

      describe "blank initialized project" $ parallel $ do

        it "can build ghc.backend" $ inTmpObInit $ \_ -> do
          run "nix-build" ["--no-out-link", "-A", "ghc.backend"]
        it "can build ghcjs.frontend" $ inTmpObInit $ \_ -> do
          run "nix-build" ["--no-out-link", "-A", "ghcjs.frontend"]

        if os == "darwin"
          then it "can build ios"     $ inTmpObInit $ \_ -> run "nix-build" ["--no-out-link", "-A", "ios.frontend"    ]
          else it "can build android" $ inTmpObInit $ \_ -> run "nix-build" ["--no-out-link", "-A", "android.frontend"]

        forM_ ["ghc", "ghcjs"] $ \compiler -> do
          let
            shell = "shells." <> compiler
            inShell cmd' = run "nix-shell" ["-A", fromString shell, "--run", cmd']
          it ("can enter "    <> shell) $ inTmpObInit $ \_ -> inShell "exit"
          it ("can build in " <> shell) $ inTmpObInit $ \_ -> inShell $ "cabal new-build --" <> fromString compiler <> " all"

        it "can build reflex project" $ inTmpObInit $ \_ -> do
          run "nix-build" []

        it "has idempotent thunk update" $ inTmpObInit $ \_ -> do
          u  <- update
          uu <- update
          assertRevEQ u uu

      describe "ob thunk pack/unpack" $ parallel $ do
        it "has thunk pack and unpack inverses" $ inTmpObInit $ \_ -> do

          e    <- commitAll
          eu   <- unpack
          eup  <- pack
          eupu <- unpack
          _    <- pack

          assertRevEQ e  eup
          assertRevEQ eu eupu
          assertRevNE e  eu

        it "can pack and unpack plain git repos" $ do
          shelly_ $ withSystemTempDirectory "git-repo" $ \dir -> do
            let repo = toTextIgnore $ dir </> ("repo" :: String)
            run_ "git" ["clone", "https://github.com/haskell/process.git", repo]
            origHash <- chdir (fromText repo) revParseHead

            run_ "ob" ["thunk", "pack", repo]
            packedFiles <- Set.fromList <$> ls (fromText repo)
            liftIO $ assertEqual "" packedFiles $ Set.fromList $ (repo </>) <$>
              ["default.nix", "github.json", ".attr-cache" :: String]

            run_ "ob" ["thunk", "unpack", repo]
            chdir (fromText repo) $ do
              unpackHash <- revParseHead
              assertRevEQ origHash unpackHash

            testThunkPack $ fromText repo

        it "aborts thunk pack when there are uncommitted files" $ inTmpObInit $ \dir -> do
          void $ unpack
          testThunkPack (dir </> thunk)

-- | Run `ob run` in the given directory (maximum of one level deep)
testObRunInDir :: Socket.PortNumber -> Socket.PortNumber -> Maybe Shelly.FilePath -> HTTP.Manager -> Sh ()
testObRunInDir p0 p1 mdir httpManager = handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
  let uri p = "http://localhost:" <> T.pack (show p) <> "/" -- trailing slash required for comparison
  writefile "config/common/route" $ uri p0
  maybe id chdir mdir $ runHandle "ob" ["run"] $ \stdout -> do
    firstUri <- handleObRunStdout httpManager stdout
    let newUri = uri p1
    when (firstUri == newUri) $ errorExit $
      "Startup URI (" <> firstUri <> ") is the same as test URI (" <> newUri <> ")"
    maybe id (\_ -> chdir "..") mdir $ alterRouteTo newUri stdout
    runningUri <- handleObRunStdout httpManager stdout
    if runningUri /= newUri
      then errorExit $ "Reloading failed: expected " <> newUri <> " but got " <> runningUri
      else exit 0

testThunkPack :: Shelly.FilePath -> Sh ()
testThunkPack path' = withTempFile (T.unpack $ toTextIgnore path') "test-file" $ \file handle -> do
  let pack' = readProcessWithExitCode "ob" ["thunk", "pack", T.unpack $ toTextIgnore path'] ""
      ensureThunkPackFails q = liftIO $ pack' >>= \case
        (code, out, err)
          | code == ExitSuccess -> fail "ob thunk pack succeeded when it should have failed"
          | q `T.isInfixOf` T.pack (out <> err) -> pure ()
          | otherwise -> fail $ "ob thunk pack failed for an unexpected reason: " <> show out <> "\nstderr: " <> err
      git = chdir path' . run "git"
  -- Untracked files
  ensureThunkPackFails "Untracked files"
  void $ git ["add", T.pack file]
  -- Uncommitted files (staged)
  ensureThunkPackFails "unsaved"
  chdir path' $ commit "test commit"
  -- Non-pushed commits in any branch
  ensureThunkPackFails "not yet pushed"
  -- Uncommitted files (unstaged)
  liftIO $ T.hPutStrLn handle "test file" >> hClose handle
  ensureThunkPackFails "modified"
  -- Existing stashes
  void $ git ["stash"]
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
handleObRunStdout :: HTTP.Manager -> Handle -> Sh Text
handleObRunStdout httpManager stdout = flip fix (ObRunState_Init, []) $ \loop (state, msgs) -> do
  liftIO (T.hGetLine stdout) >>= \t -> case state of
    ObRunState_Init
      | "Running test..." `T.isPrefixOf` t -> loop (ObRunState_BackendStarted, msgs)
    ObRunState_Startup
      | t == "backend stopped; make a change to your code to reload" -> loop (ObRunState_Startup, msgs)
      -- | Just port <- "Backend running on port " `T.stripPrefix` t -> loop $ ObRunState_BackendStarted port
      | not (T.null t) -> errorExit $ "Startup: " <> t -- If theres any other output here, startup failed
    ObRunState_BackendStarted
      | Just uri <- "Frontend running on " `T.stripPrefix` t -> do
        obRunCheck httpManager stdout uri
        pure uri
      | not (T.null t) -> errorExit $ "Started: " <> t -- If theres any other output here, startup failed
    _ | "Failed" `T.isPrefixOf` t -> errorExit $ "ob run failed: " <> T.unlines (reverse $ t : msgs)
      | otherwise -> loop (state, t : msgs)

-- | Make requests to frontend/backend servers to check they are working properly
obRunCheck :: HTTP.Manager -> Handle -> Text -> Sh ()
obRunCheck httpManager _stdout frontendUri = do
  let req uri = liftIO $ HTTP.parseRequest (T.unpack uri) >>= flip HTTP.httpLbs httpManager
  req frontendUri >>= \r -> when (HTTP.responseStatus r /= HTTP.ok200) $ errorExit $
    "Request to frontend server failed: " <> T.pack (show r)

getFreePorts :: Int -> IO [Socket.PortNumber]
getFreePorts 0 = pure []
getFreePorts n = Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) Socket.close $ \s -> (:) <$> Socket.socketPort s <*> getFreePorts (n - 1)
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      pure sock

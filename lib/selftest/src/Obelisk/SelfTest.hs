{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.SelfTest where

import Control.Exception (Exception, throw, bracket)
import Control.Monad
import Control.Monad.IO.Class
import Data.Bool (bool)
import Data.Function (fix)
import Data.Maybe (isJust)
import Data.Semigroup (Semigroup, (<>))
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import Shelly
import System.Directory (withCurrentDirectory)
import System.Environment
import System.Exit (ExitCode (..), exitWith)
import System.Info
import System.IO (Handle, hClose)
import System.IO.Temp
import System.Timeout
import Test.Hspec
import Test.HUnit.Base
import System.Process (readProcessWithExitCode)

import Obelisk.Run (getConfigRoute)

data ObRunState
  = ObRunState_Init
  | ObRunState_Startup
  | ObRunState_BackendStarted Text -- Port of backend
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

main :: IO ()
main = do
  isVerbose <- getArgs >>= \case
    ["-v"] -> pure True
    []     -> putStrLn "Tests may take longer to run if there are unbuilt derivations: use -v for verbose output" *> pure False
    _      -> putStrLn "Usage: selftest [-v]" *> exitWith (ExitFailure 2)
  let verbosity = bool silently verbosely isVerbose
  obeliskImpl <- fromString <$> getEnv "OBELISK_IMPL"
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings
  withSystemTempDirectory "blank-project" $ \blankProject ->
    hspec $ do
      let shelly_ = void . shelly . verbosity

          inProj :: Sh a -> IO ()
          inProj = shelly_ . chdir (fromString blankProject)

          inTmp :: (Shelly.FilePath -> Sh a) -> IO ()
          inTmp f = shelly_ . withSystemTempDirectory "ob-init" $ (chdir <*> f) . fromString

          assertRevEQ a b = liftIO . assertEqual "" ""        =<< diff a b
          assertRevNE a b = liftIO . assertBool  "" . (/= "") =<< diff a b

          revParseHead = T.strip <$> run "git" ["rev-parse", "HEAD"]

          commitAll = do
            run "git" ["add", "."]
            commit "checkpoint"
            revParseHead

          thunk  = ".obelisk/impl"
          update = run "ob" ["thunk", "update", thunk] >> commitAll
          pack   = run "ob" ["thunk", "pack",   thunk] >> commitAll
          unpack = run "ob" ["thunk", "unpack", thunk] >> commitAll

          diff a b = run "git" ["diff", a, b]

      describe "ob init" $ do
        it "works with default impl"       $ inTmp $ \_ -> run "ob" ["init"]
        it "works with master branch impl" $ inTmp $ \_ -> run "ob" ["init", "--branch", "master"]
        it "works with symlink"            $ inTmp $ \_ -> run "ob" ["init", "--symlink", obeliskImpl]

        it "doesn't create anything when given an invalid impl" $ inTmp $ \tmp -> do
          errExit False $ run "ob" ["init", "--symlink", "/dev/null"]
          ls tmp >>= liftIO . assertEqual "" []

        it "produces a valid route config" $ inTmp $ \tmp -> do
          run "ob" ["init"]
          liftIO $ withCurrentDirectory (T.unpack $ toTextIgnore tmp) $ getConfigRoute `shouldNotReturn` Nothing

      describe "ob run" $ do
        it "works in root directory" $ inTmp $ \_ -> do
          run "ob" ["init"]
          testObRunInDir Nothing httpManager
        it "works in sub directory" $ inTmp $ \_ -> do
          run "ob" ["init"]
          testObRunInDir (Just "frontend") httpManager

      describe "obelisk project" $ parallel $ do
        it "can build obelisk command"  $ shelly_ $ run "nix-build" ["-A", "command" , obeliskImpl]
        it "can build obelisk skeleton" $ shelly_ $ run "nix-build" ["-A", "skeleton", obeliskImpl]
        it "can build obelisk shell"    $ shelly_ $ run "nix-build" ["-A", "shell",    obeliskImpl]
        it "can build everything"       $ shelly_ $ run "nix-build" [obeliskImpl]

      describe "blank initialized project" $ do
        it "can be created" $ inProj $ do
          run "ob" ["init"]
          run "git" ["init"]
          commitAll

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

        it "can build reflex project" $ inProj $ do
          run "nix-build" []

        it "has idempotent thunk update" $ inProj $ do
          u  <- update
          uu <- update
          assertRevEQ u uu

      describe "ob thunk pack/unpack" $ do
        it "has thunk pack and unpack inverses" $ inProj $ do
          e    <- commitAll
          eu   <- unpack
          eup  <- pack
          eupu <- unpack
          _    <- pack

          assertRevEQ e  eup
          assertRevEQ eu eupu
          assertRevNE e  eu

        it "can pack and unpack plain git repos" $ do
          withSystemTempDirectory "git-repo" $ \dir -> shelly_ $ do
            let repo = toTextIgnore $ dir </> ("repo" :: String)
            run_ "git" ["clone", "https://git.haskell.org/packages/primitive.git", repo]
            origHash <- chdir (fromText repo) revParseHead

            run_ "ob" ["thunk", "pack", repo]
            packedFiles <- Set.fromList <$> ls (fromText repo)
            liftIO $ assertEqual "" packedFiles $
              Set.fromList $ (repo </>) <$> ["default.nix", "git.json", ".attr-cache" :: String]

            run_ "ob" ["thunk", "unpack", repo]
            chdir (fromText repo) $ do
              unpackHash <- revParseHead
              assertRevEQ origHash unpackHash

            testThunkPack $ fromText repo

        it "aborts thunk pack when there are uncommitted files" $ inProj $ do
          unpack
          testThunkPack (blankProject </> thunk)

-- | Run `ob run` in the given directory (maximum of one level deep)
testObRunInDir :: Maybe Shelly.FilePath -> HTTP.Manager -> Sh ()
testObRunInDir mdir httpManager = handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
  let uri p = "http://localhost:" <> T.pack (show p) <> "/" -- trailing slash required for comparison
  writefile "config/common/route" . uri =<< liftIO getFreePort -- Use a free port initially
  maybe id chdir mdir $ runHandle "ob" ["run"] $ \stdout -> do
    freePort <- liftIO getFreePort
    (_, firstUri) <- handleObRunStdout httpManager stdout
    let newUri = uri freePort
    when (firstUri == newUri) $ errorExit $
      "Startup URI (" <> firstUri <> ") is the same as test URI (" <> newUri <> ")"
    maybe id (\_ -> chdir "..") mdir $ alterRouteTo newUri stdout
    (_, runningUri) <- handleObRunStdout httpManager stdout
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
  chdir path $ commit "test commit"
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
handleObRunStdout :: HTTP.Manager -> Handle -> Sh (Text, Text)
handleObRunStdout httpManager stdout = flip fix ObRunState_Init $ \loop state -> do
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

getFreePort :: IO Socket.PortNumber
getFreePort = Socket.withSocketsDo $ do
  addr:_ <- Socket.getAddrInfo (Just Socket.defaultHints) (Just "127.0.0.1") (Just "0")
  bracket (open addr) Socket.close Socket.socketPort
  where
    open addr = do
      sock <- Socket.socket (Socket.addrFamily addr) (Socket.addrSocketType addr) (Socket.addrProtocol addr)
      Socket.bind sock (Socket.addrAddress addr)
      pure sock

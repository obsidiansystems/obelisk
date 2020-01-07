{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
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
import System.Directory (withCurrentDirectory, getDirectoryContents)
import System.Environment
import System.Exit (ExitCode (..))
import System.FilePath (replaceBaseName, takeBaseName)
import qualified System.Info
import System.IO (Handle, hClose)
import System.IO.Temp
import System.Process (readProcessWithExitCode, CreateProcess(cwd), readCreateProcessWithExitCode, proc)
import Test.Hspec
import Test.HUnit.Base

import Obelisk.CliApp hiding (runCli, readCreateProcessWithExitCode)
import qualified Obelisk.CliApp as CliApp
import Obelisk.ExecutableConfig.Lookup (getConfigs)
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
shellyOb :: MonadIO m => (Sh a -> Sh a) -> Sh a -> m a
shellyOb f obTest = shelly $ f obTest

-- Set "ob" in a single place so it can be replaced with a
-- link to obelisk in the nix store in the future,
-- and avoid PATH hacking before calling this script.
ob :: FilePath
ob = "ob"

convertRunner :: (String -> [Text] -> a) -> String -> Bool -> [Text] -> a
convertRunner runner command isVerbose args = runner command $ (if isVerbose then ("-v" :) else id) args

main :: IO ()
main = do
  -- Note: you can pass hspec arguments as well, eg: `-m <pattern>`
  isVerbose <- elem "-v" <$> getArgs
  unless isVerbose $
    putStrLn "Tests may take longer to run if there are unbuilt derivations: use -v for verbose output"
  let verbosity = bool silently verbosely isVerbose
      nixBuild args = run "nix-build" ("--no-out-link" : args)
  obeliskImplRaw <- fromString <$> getEnv "OBELISK_IMPL"
  let runOb_ = convertRunner run_ ob isVerbose
  let runOb = convertRunner run ob isVerbose
  let testObRunInDir' = convertRunner testObRunInDir ob isVerbose ["run"]
  let testThunkPack' = convertRunner testThunkPack ob isVerbose []
  let
    withObeliskImpl f =
      withSystemTempDirectory "obelisk-impl-copy" $ \(fromString -> obeliskImpl) -> do
        void . shellyOb verbosity $ chdir obeliskImpl $ do
          user <- T.strip <$> run "whoami" []
          run_ "cp" ["-rT", toTextIgnore obeliskImplRaw, toTextIgnore obeliskImpl]
          run_ "chown" ["-R", user, toTextIgnore obeliskImpl]
          run_ "chmod" ["-R", "g-rw,o-rw", toTextIgnore obeliskImpl]
        f obeliskImpl

    withInitCache f obeliskImpl =
      withSystemTempDirectory "init Cache λ" $ \initCache -> do
        -- Setup the ob init cache
        void . shellyOb verbosity $ chdir initCache $ do
          runOb_ ["init", "--symlink", toTextIgnore obeliskImpl]
          run_ "git" ["init"]

        f initCache

    withObeliskImplAndInitCache f =
      withObeliskImpl $ \impl -> withInitCache (f impl) impl

  httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  withObeliskImplAndInitCache $ \obeliskImpl initCache ->
    hspec $ parallel $ do
      let shelly_ = void . shellyOb verbosity

          inTmp :: (FilePath -> Sh a) -> IO ()
          inTmp f = withTmp (chdir <*> f)

          withTmp f = shelly_ . withSystemTempDirectory "test λ" $ f . fromString

          inTmpObInit f = inTmp $ \dir -> do
            run_ "cp" ["-rT", fromString initCache, toTextIgnore dir]
            f dir

          -- To be used in tests that change the obelisk impl directory
          inTmpObInitWithImplCopy f = inTmpObInit $ \dir ->
            withObeliskImpl $ \(fromString -> implCopy) -> do
              run_ "rm" [thunk]
              run_ "ln" ["-s", implCopy, thunk]
              f dir

          assertRevEQ a b = liftIO . assertEqual "" ""        =<< diff a b
          assertRevNE a b = liftIO . assertBool  "" . (/= "") =<< diff a b

          revParseHead = T.strip <$> run "git" ["rev-parse", "HEAD"]

          commitAll = do
            run_ "git" ["add", "."]
            commit "checkpoint"
            revParseHead

          thunk  = ".obelisk/impl"
          update = runOb ["thunk", "update", thunk] >> commitAll
          pack   = runOb ["thunk", "pack",   thunk] >> commitAll
          unpack = runOb ["thunk", "unpack", thunk] >> commitAll

          diff a b = run "git" ["diff", a, b]

      describe "ob init" $ parallel $ do
        it "works with default impl"       $ inTmp $ \_ -> runOb ["init"]
        it "works with master branch impl" $ inTmp $ \_ -> runOb ["init", "--branch", "master"]
        it "works with symlink"            $ inTmp $ \_ -> runOb ["init", "--symlink", toTextIgnore obeliskImpl]
        it "doesn't silently overwrite existing files" $ withSystemTempDirectory "ob-init λ" $ \dir -> do
          let p force = (System.Process.proc ob $ "--no-handoff" : "-v" : "init" : ["--force"|force]) { cwd = Just dir }
          (ExitSuccess, _, _) <- readCreateProcessWithExitCode (p False) ""
          (ExitFailure _, _, _) <- readCreateProcessWithExitCode (p False) ""
          (ExitSuccess, _, _) <- readCreateProcessWithExitCode (p True) ""
          pure ()

        it "doesn't create anything when given an invalid impl" $ inTmp $ \tmp -> do
          void $ errExit False $ runOb ["init", "--symlink", "/dev/null"]
          ls tmp >>= liftIO . assertEqual "" []

        it "produces a valid route config" $ inTmpObInit $ \tmp -> liftIO $
          withCurrentDirectory (T.unpack $ toTextIgnore tmp) $ do
            configs <- getConfigs
            return (either (const Nothing) Just $ getConfigRoute configs) `shouldNotReturn` Nothing

      -- These tests fail with "Could not find module 'Obelisk.Generated.Static'"
      -- when not run by 'nix-build --attr selftest'
      describe "ob run" $ {- NOT parallel $ -} do
        it "works in root directory" $ inTmpObInit $ \_ -> testObRunInDir' Nothing httpManager
        it "works in sub directory" $ inTmpObInit $ \_ -> testObRunInDir' (Just "frontend") httpManager
        it "works with differently named cabal files" $ inTmpObInit $ \_ -> do
          changeCabalPackageName "backend/backend.cabal" "new-backend"
          testObRunInDir' Nothing httpManager

      describe "obelisk project" $ parallel $ do
        it "can build obelisk command"  $ inTmpObInit $ \_ -> nixBuild ["-A", "command" , toTextIgnore obeliskImpl]
        it "can build obelisk skeleton" $ inTmpObInit $ \_ -> nixBuild ["-A", "skeleton", toTextIgnore obeliskImpl]
        it "can build obelisk shell"    $ inTmpObInit $ \_ -> nixBuild ["-A", "shell",    toTextIgnore obeliskImpl]
        it "can build everything"       $ inTmpObInit $ \_ -> nixBuild [toTextIgnore obeliskImpl]

      describe "blank initialized project" $ parallel $ do

        it "can build ghc.backend" $ inTmpObInit $ \_ -> nixBuild ["-A", "ghc.backend"]
        it "can build ghcjs.frontend" $ inTmpObInit $ \_ -> nixBuild ["-A", "ghcjs.frontend"]

        if System.Info.os == "darwin"
          then it "can build ios" $ inTmpObInit $ \_ -> nixBuild ["-A", "ios.frontend"]
          else it "can build android after accepting license" $ inTmpObInit $ \dir -> do
            let defaultNixPath = dir </> ("default.nix" :: FilePath)
            writefile defaultNixPath
              =<< T.replace
                "# config.android_sdk.accept_license = false;"
                "config.android_sdk.accept_license = true;"
              <$> readfile defaultNixPath
            nixBuild ["-A", "android.frontend"]

        forM_ ["ghc", "ghcjs"] $ \compiler -> do
          let
            shellName = "shells." <> compiler
            inShell cmd' = run "nix-shell" ["default.nix", "-A", fromString shellName, "--run", cmd']
          it ("can enter "    <> shellName) $ inTmpObInit $ \_ -> inShell "exit"
          it ("can build in " <> shellName) $ inTmpObInit $ \_ -> inShell $ "cabal new-build --" <> fromString compiler <> " all"

        it "has idempotent thunk update" $ inTmpObInitWithImplCopy $ \_ -> do
          _  <- pack
          u  <- update
          uu <- update
          assertRevEQ u uu

      describe "ob thunk pack/unpack" $ parallel $ do
        it "has thunk pack and unpack inverses" $ inTmpObInitWithImplCopy $ \_ -> do

          _    <- pack
          e    <- commitAll
          eu   <- unpack
          eup  <- pack
          eupu <- unpack
          _    <- pack

          assertRevEQ e  eup
          assertRevEQ eu eupu
          assertRevNE e  eu

        it "unpacks the correct branch" $ withTmp $ \dir -> do
          let branch = "master"
          run_ "git" ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", branch]
          runOb_ ["thunk", "pack", toTextIgnore dir]
          runOb_ ["thunk", "unpack", toTextIgnore dir]
          branch' <- chdir dir $ run "git" ["rev-parse", "--abbrev-ref", "HEAD"]
          liftIO $ assertEqual "" branch (T.strip branch')

        it "can pack and unpack plain git repos" $
          shelly_ $ withSystemTempDirectory "git-repo" $ \dir -> do
            let repo = toTextIgnore $ dir </> ("repo" :: String)
            run_ "git" ["clone", "https://github.com/haskell/process.git", repo]
            origHash <- chdir (fromText repo) revParseHead

            runOb_ ["thunk", "pack", repo]
            packedFiles <- Set.fromList <$> ls (fromText repo)
            liftIO $ assertEqual "" packedFiles $ Set.fromList $ (repo </>) <$>
              ["default.nix", "github.json", ".attr-cache" :: String]

            runOb_ ["thunk", "unpack", repo]
            chdir (fromText repo) $ do
              unpackHash <- revParseHead
              assertRevEQ origHash unpackHash

            testThunkPack' $ fromText repo

        it "aborts thunk pack when there are uncommitted files" $ inTmpObInit $ \dir -> do
          testThunkPack' (dir </> thunk)

      describe "ob thunk update --branch" $ parallel $ do
        it "can change a thunk to the latest version of a desired branch" $ withTmp $ \dir -> do
          let branch1 = "master"
              branch2 = "develop"
          run_ "git" ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", branch1]
          runOb_ ["thunk" , "pack", toTextIgnore dir]
          runOb_ ["thunk", "update", toTextIgnore dir, "--branch", branch2]

        it "doesn't create anything when given an invalid branch" $ withTmp $ \dir -> do
          let checkDir dir' = liftIO $ getDirectoryContents $ T.unpack $ toTextIgnore dir'
          run_ "git" ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", "master"]
          runOb_ ["thunk" , "pack", toTextIgnore dir]
          startingContents <- checkDir dir
          void $ errExit False $ runOb ["thunk", "update", toTextIgnore dir, "--branch", "dumble-palooza"]
          checkDir dir >>= liftIO . assertEqual "" startingContents


-- | Run `ob run` in the given directory (maximum of one level deep)
testObRunInDir :: String -> [Text] -> Maybe FilePath -> HTTP.Manager -> Sh ()
testObRunInDir cmd extraArgs mdir httpManager = handle_sh (\case ExitSuccess -> pure (); e -> throw e) $ do
  [p0, p1] <- liftIO $ getFreePorts 2
  let uri p = "http://localhost:" <> T.pack (show p) <> "/" -- trailing slash required for comparison
  writefile "config/common/route" $ uri p0
  maybe id chdir mdir $ runHandle cmd extraArgs $ \stdout -> do
    firstUri <- handleObRunStdout httpManager stdout
    let newUri = uri p1
    when (firstUri == newUri) $ errorExit $
      "Startup URI (" <> firstUri <> ") is the same as test URI (" <> newUri <> ")"
    maybe id (\_ -> chdir "..") mdir $ alterRouteTo newUri stdout
    runningUri <- handleObRunStdout httpManager stdout
    if runningUri /= newUri
      then errorExit $ "Reloading failed: expected " <> newUri <> " but got " <> runningUri
      else exit 0

testThunkPack :: String -> [Text] -> FilePath -> Sh ()
testThunkPack command args path' = withTempFile (T.unpack $ toTextIgnore path') "test-file" $ \file handle -> do
  let pack' = readProcessWithExitCode command ["thunk", "pack", T.unpack $ toTextIgnore path'] ""
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
handleObRunStdout httpManager stdout = flip fix (ObRunState_Init, []) $ \loop (state, msgs) ->
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

-- | Rename a cabal file and do a really dumb, brittle search/replace in its content to update the name.
changeCabalPackageName :: FilePath -> Text -> Sh ()
changeCabalPackageName cabalFile newName = do
  contents <- readfile cabalFile
  writefile (replaceBaseName cabalFile (T.unpack newName)) $
    T.replace (" " <> T.pack (takeBaseName cabalFile)) (" " <> newName) contents -- WARNING: Super brittle
  rm cabalFile

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

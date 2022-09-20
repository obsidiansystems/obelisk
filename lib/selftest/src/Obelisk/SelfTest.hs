{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module Obelisk.SelfTest where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, throw, try)
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (for_)
import Data.Function (fix)
import Data.List (isInfixOf)
import qualified Data.Map as Map
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Socket
import Shelly
import System.Directory (getCurrentDirectory, withCurrentDirectory, getDirectoryContents)
import System.Environment
import System.Exit (ExitCode (..))
import System.FilePath (addTrailingPathSeparator)
import qualified System.Info
import System.IO (Handle, hClose, hIsEOF, hGetContents)
import System.IO.Temp
import System.Process (readProcessWithExitCode)
import System.Which (staticWhich)
import Test.Hspec
import Test.HUnit.Base

import Obelisk.ExecutableConfig.Lookup (getConfigs)
import Obelisk.Run (getConfigRoute)

data ObRunState
  = ObRunState_Init
  | ObRunState_Startup
  | ObRunState_BackendStarted
  deriving (Eq, Show)

cpPath :: FilePath
cpPath = $(staticWhich "cp")

cabalPath :: FilePath
cabalPath = $(staticWhich "cabal")

gitPath :: FilePath
gitPath = $(staticWhich "git")

chownPath :: FilePath
chownPath = $(staticWhich "chown")

chmodPath :: FilePath
chmodPath = $(staticWhich "chmod")

whoamiPath :: FilePath
whoamiPath = $(staticWhich "whoami")

nixInstantiatePath :: FilePath
nixInstantiatePath = $(staticWhich "nix-instantiate")

nixBuildPath :: FilePath
nixBuildPath = $(staticWhich "nix-build")

nixPrefetchGitPath :: FilePath
nixPrefetchGitPath = $(staticWhich "nix-prefetch-git")

lnPath :: FilePath
lnPath = $(staticWhich "ln")

rmPath :: FilePath
rmPath = $(staticWhich "rm")

rsyncPath :: FilePath
rsyncPath = $(staticWhich "rsync")

mvPath :: FilePath
mvPath = $(staticWhich "mv")

sedPath :: FilePath
sedPath = $(staticWhich "sed")

gitUserConfig :: [Text]
gitUserConfig = ["-c", "user.name=Obelisk Selftest", "-c", "user.email=noreply@example.com"]

commit :: Text -> Sh ()
commit msg = void $ run gitPath $ gitUserConfig <> [ "commit"
  , "--no-gpg-sign"
  , "--allow-empty"
  , "-m"
  , msg
  ]

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

augmentWithVerbosity :: (String -> [Text] -> a) -> String -> Bool -> [Text] -> a
augmentWithVerbosity runner executable isVerbose args = runner executable $ (if isVerbose then ("-v" :) else id) args

isolatedGitShell :: Bool -> FilePath -> (([Text] -> Sh Text) -> Sh a) -> IO a
isolatedGitShell isVerbose dir f = shelly $ bool silently verbosely isVerbose $ do
  setenv "HOME" "/dev/null"
  setenv "GIT_CONFIG_NOSYSTEM" "1"
  _ <- git ["init"]
  _ <- git ["config", "user.name", "SelfTest"]
  _ <- git ["config", "user.email", "self@test"]
  f git
  where
    git args = run gitPath $ ["-C", toTextIgnore dir] <> args

-- | Copies a git repo to a new location and "resets" the git history to include
-- exactly one commit with all files added. It then restricts writing and reading
-- for group and user to make the repo ideal for being a valid git remote for thunks.
--
-- Using this allows dirty repos to be used as git remotes during the test since 'git clone'ing
-- a dirty repo will not include the uncommitted changes.
copyForGitRemote :: Bool -> FilePath -> FilePath -> IO ()
copyForGitRemote isVerbose origDir copyDir = isolatedGitShell isVerbose copyDir $ \git -> do
  run_ rsyncPath
    [ "-r", "--no-perms", "--no-owner", "--no-group", "--exclude", ".git"
    , toTextIgnore (addTrailingPathSeparator origDir), toTextIgnore copyDir
    ]
  _ <- git ["add", "--all"]
  _ <- git ["commit", "-m", "Copy repo"]
  run_ chmodPath ["-R", "u-w,g-rw,o-rw", toTextIgnore copyDir] -- Freeze this state

main :: IO ()
main = do
  -- Note: you can pass hspec arguments as well, eg: `-m <pattern>`
  isVerbose <- elem "-v" <$> getArgs
  unless isVerbose $
    putStrLn "Tests may take longer to run if there are unbuilt derivations: use -v for verbose output"

  obeliskImplDirtyReadOnly <- getCurrentDirectory
  httpManager <- HTTP.newManager HTTP.defaultManagerSettings

  withSystemTempDirectory "obelisk-repo-git-remote" $ \copyDir -> do
    copyForGitRemote isVerbose obeliskImplDirtyReadOnly copyDir
    main' isVerbose httpManager copyDir

main' :: Bool -> HTTP.Manager -> FilePath -> IO ()
main' isVerbose httpManager obeliskRepoReadOnly = withInitCache $ \initCache -> hspec $ parallel $ do
  let
    inTmpObInit' dirname f = inTmp' dirname $ \dir -> do
      run_ cpPath ["-rT", fromString initCache, toTextIgnore dir]
      f dir
    inTmpObInit = inTmpObInit' defaultTmpDirName

    -- To be used in tests that change the obelisk impl directory
    inTmpObInitWithImplCopy f = inTmpObInit $ \dir ->
      withObeliskImplClean $ \(fromString -> implClean) -> do
        run_ rmPath [thunk]
        run_ lnPath ["-s", implClean, thunk]
        f dir

  describe "ob init" $ parallel $ do
    it "works with default impl"       $ inTmp $ \_ -> runOb ["init"]
    it "works with master branch impl" $ inTmp $ \_ -> runOb ["init", "--branch", "master"]
    it "works with symlink"            $ inTmp $ \_ -> runOb ["init", "--symlink", toTextIgnore obeliskRepoReadOnly]
    it "doesn't silently overwrite existing files" $ inTmp $ \_ -> do
      let p force = errExit False $ do
            run_ ob $ "--no-handoff" : "-v" : "init" : ["--force"|force]
            (== 0) <$> lastExitCode

      True <- p False
      False <- p False
      True <- p True
      pure ()

    it "doesn't create anything when given an invalid impl" $ inTmp $ \tmp -> do
      void $ errExit False $ runOb ["init", "--symlink", "/dev/null"]
      ls tmp >>= liftIO . assertEqual "" []

    it "produces a valid route config" $ inTmpObInit $ \tmp -> liftIO $
      withCurrentDirectory (T.unpack $ toTextIgnore tmp) $ do
        configs <- getConfigs
        return (either (const Nothing) Just $ getConfigRoute configs) `shouldNotReturn` Nothing

    it "can unpack and repack .obelisk/impl after init with master branch impl" $ inTmp $ \_ -> do
      runOb_ ["init", "--branch", "master"]
      runOb_ ["thunk", "unpack", ".obelisk/impl"]
      runOb_ ["thunk", "pack", ".obelisk/impl"]

  -- These tests fail with "Could not find module 'Obelisk.Generated.Static'"
  -- when not run by 'nix-build --attr selftest'
  describe "ob run" $ {- NOT parallel $ -} do
    it "works in root directory" $ inTmpObInit $ \_ -> testObRunInDir' Nothing httpManager
    it "works in sub directory" $ inTmpObInit $ \_ -> testObRunInDir' (Just "frontend") httpManager
    it "can read external TLS certificates in root directory" $ inTmpObInit $ \testDir -> testObRunCert' testDir Nothing
    it "can read external TLS certificates in sub directory" $ inTmpObInit $ \testDir -> testObRunCert' testDir (Just "frontend")
    it "complains when static files are missing in root directory" $ inTmpObInit $ const $ testObRunInDirWithMissingStaticFile' Nothing
    it "complains when static files are missing in sub directory" $ inTmpObInit $ const $ testObRunInDirWithMissingStaticFile' (Just "frontend")
    it "complains when static filepaths are missing in root directory" $ do
      inTmpObInit $ const $ testObRunInDirWithMissingStaticFilePath' Nothing
    it "complains when static filepaths are missing in sub directory" $ do
      inTmpObInit $ const $ testObRunInDirWithMissingStaticFilePath' (Just "frontend")
    it "fails when given an invalid Cabal file" $ inTmpObInit $ testObRunWithInvalidCabalFile ob

    it "respects the port given on the command line" $ inTmpObInit $ \testDir -> do
      [port] <- liftIO $ getFreePorts 1
      maskExitSuccess $ runHandles ob ["run", "-p", T.pack (show port)] [] $ \_stdin stdout stderr -> do
        uri <- handleObRunStdout httpManager stdout stderr
        -- Make sure obelisk logs the right thing
        unless (T.pack (show port ++ "/") `T.isSuffixOf` T.strip uri) $
          error $ "Expected the URI to end in " ++ show port ++ " but it ended in " ++ T.unpack uri
        -- But also verify that we can actually reach the server in the
        -- given path, rather than just listening for the log URL
        let req = liftIO $ try @HTTP.HttpException $ HTTP.parseRequest (T.unpack uri) >>= flip HTTP.httpLbs httpManager
        req >>= \case
          Right r | HTTP.responseStatus r == HTTP.ok200 -> exit 0
          e -> errorExit $ "Request to ob run failed: " <> T.pack (show e)
        exit 0
    it "specify default frontend action" $ inTmpObInit $ \_ -> do
      testObRunInDirWithArgs' ["--frontend", "Frontend.frontend"] Nothing httpManager
    it "specify default backend action" $ inTmpObInit $ \_ -> do
      testObRunInDirWithArgs' ["--backend", "Backend.backend"] Nothing httpManager
    it "specify default frontend and backend actions" $ inTmpObInit $ \_ -> do
      testObRunInDirWithArgs' ["--frontend", "Frontend.frontend", "--backend", "Backend.backend"] Nothing httpManager
    it "specify alternative frontend action" $ inTmpObInit $ \_ -> do
      run_ sedPath ["-e", "s/^frontend/frontendAlt/", "-i", "frontend/src/Frontend.hs"]
      testObRunInDirWithArgs' ["--frontend", "Frontend.frontendAlt"] Nothing httpManager
    it "specify alternative backend action" $ inTmpObInit $ \_ -> do
      run_ sedPath ["-e", "s/^backend/backendAlt/", "-i", "backend/src/Backend.hs"]
      testObRunInDirWithArgs' ["--backend", "Backend.backendAlt"] Nothing httpManager
    it "specify alternative frontend and backend actions" $ inTmpObInit $ \_ -> do
      run_ sedPath ["-e", "s/^frontend/frontendAlt/", "-i", "frontend/src/Frontend.hs"]
      run_ sedPath ["-e", "s/^backend/backendAlt/", "-i", "backend/src/Backend.hs"]
      testObRunInDirWithArgs' ["--frontend", "Frontend.frontendAlt", "--backend", "Backend.backendAlt"] Nothing httpManager
    it "works in sub-directory with alternative frontend and backend actions" $ inTmpObInit $ \_ -> do
      run_ sedPath ["-e", "s/^frontend/frontendAlt/", "-i", "frontend/src/Frontend.hs"]
      run_ sedPath ["-e", "s/^backend/backendAlt/", "-i", "backend/src/Backend.hs"]
      testObRunInDirWithArgs' ["--frontend", "Frontend.frontendAlt", "--backend", "Backend.backendAlt"] (Just "frontend") httpManager

  describe "ob repl" $ do
    it "accepts stdin commands" $ inTmpObInit $ \_ -> do
      setStdin "print 3\n:q"
      output <- runOb ["repl"]
      liftIO $ assertBool "" $
        [ "*Obelisk.Run Obelisk.Run Frontend Backend> 3"
        , "*Obelisk.Run Obelisk.Run Frontend Backend> Leaving GHCi."
        ] `isInfixOf` T.lines (T.strip output)
    it "works with custom Prelude" $ inTmpObInit $ \_ -> do
      writefile "common/src/Prelude.hs"
        "{-# LANGUAGE PackageImports #-} module Prelude (module X) where import \"base\" Prelude as X"
      setStdin ":q"
      output <- runOb ["repl"]
      liftIO $ assertBool "" $
        "*Obelisk.Run Obelisk.Run Frontend Backend> Leaving GHCi." `T.isInfixOf` output

  describe "obelisk project" $ parallel $ do
    it "can build obelisk command"  $ inTmpObInit $ \_ -> nixBuild ["-A", "command" , toTextIgnore obeliskRepoReadOnly]
    it "can build obelisk skeleton" $ inTmpObInit $ \_ -> nixBuild ["-A", "skeleton", toTextIgnore obeliskRepoReadOnly]
    it "can build obelisk shell"    $ inTmpObInit $ \_ -> nixBuild ["-A", "shell",    toTextIgnore obeliskRepoReadOnly]
    it "can build everything"       $ inTmpObInit $ \_ -> nixBuild [toTextIgnore obeliskRepoReadOnly]

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
        inShell cmd' = run_ "nix-shell" ["default.nix", "-A", fromString shellName, "--run", cmd']
      it ("can enter "    <> shellName) $ inTmpObInit $ \_ -> inShell "exit"
      -- NOTE: We override the temporary directory name because cabal has a bug preventing new-build from working
      -- in a path that has unicode characters.
      it ("can build in " <> shellName) $ inTmpObInit' "test" $ \_ -> inShell $
          T.pack cabalPath <> " --version; " <> T.pack cabalPath <> " new-build --" <> T.pack compiler <> " all"

    it "has idempotent thunk update" $ inTmpObInitWithImplCopy $ \_ -> do
      _  <- pack
      u  <- update
      uu <- update
      assertRevEQ u uu

    it "can run 'ob doc'" $ inTmpObInit $ \_ -> runOb_ ["doc", "reflex"]

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
      run_ gitPath ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", branch]
      runOb_ ["thunk", "pack", toTextIgnore dir]
      runOb_ ["thunk", "unpack", toTextIgnore dir]
      branch' <- run gitPath ["-C", toTextIgnore $ dir </> unpackedDirName, "rev-parse", "--abbrev-ref", "HEAD"]
      liftIO $ assertEqual "" branch (T.strip branch')

    it "can pack and unpack plain git repos" $
      shelly_ $ withSystemTempDirectory "git-repo" $ \dir -> do
        let repo = toTextIgnore $ dir </> ("repo" :: FilePath)
        run_ gitPath ["clone", "https://github.com/haskell/process.git", repo]
        origHash <- chdir (fromText repo) revParseHead

        runOb_ ["thunk", "pack", repo]
        packedFiles <- Set.fromList <$> ls (fromText repo)
        liftIO $ assertEqual "" packedFiles $ Set.fromList $ (repo </>) <$>
          ["default.nix", "thunk.nix", "github.json" :: FilePath]

        runOb_ ["thunk", "unpack", repo]
        chdir (fromText repo </> unpackedDirName) $ do
          unpackHash <- revParseHead
          assertRevEQ origHash unpackHash

        testThunkPack' $ fromText repo

    it "aborts thunk pack when there are uncommitted files" $ inTmpObInitWithImplCopy $ \dir -> do
      testThunkPack' (dir </> thunk)

    it "works on legacy git thunks" $ testLegacyGitThunks isVerbose

  describe "ob thunk update --branch" $ parallel $ do
    it "can change a thunk to the latest version of a desired branch" $ withTmp $ \dir -> do
      let branch1 = "master"
          branch2 = "develop"
      run_ gitPath ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", branch1]
      runOb_ ["thunk" , "pack", toTextIgnore dir]
      runOb_ ["thunk", "update", toTextIgnore dir, "--branch", branch2]

    it "doesn't create anything when given an invalid branch" $ withTmp $ \dir -> do
      let checkDir dir' = liftIO $ getDirectoryContents $ T.unpack $ toTextIgnore dir'
      run_ gitPath ["clone", "https://github.com/reflex-frp/reflex.git", toTextIgnore dir, "--branch", "master"]
      runOb_ ["thunk" , "pack", toTextIgnore dir]
      startingContents <- checkDir dir
      void $ errExit False $ runOb ["thunk", "update", toTextIgnore dir, "--branch", "dumble-palooza"]
      checkDir dir >>= liftIO . assertEqual "" startingContents

  describe "ob shell" $ parallel $ do
    it "works with --" $ inTmpObInit $ \_ -> do
      output <- runOb ["shell", "--", "ghc-pkg", "list"]
      liftIO $ assertBool "Unexpected output from ob shell" $ ("Cabal-" `T.isInfixOf` output) && ("ghc-" `T.isInfixOf` output)

  describe "ob hoogle" $ {- NOT parallel -} do
    it "starts a hoogle server on the given port" $ inTmpObInit $ \_ -> do
      [p0] <- liftIO $ getFreePorts 1
      maskExitSuccess $ runHandle "ob" ["hoogle", "--port", T.pack (show p0)] $ \stdout -> flip fix Nothing $ \loop -> \case
        Nothing -> do -- Still waiting for initial signal that the server has started
          ln <- liftIO $ T.hGetLine stdout
          let search = "Server starting on port " <> T.pack (show p0)
          case search `T.isInfixOf` ln of
            False -> loop Nothing -- keep waiting
            True -> loop $ Just 10
        Just (n :: Int) -> do -- Server has started and we have n attempts left
          let req uri = liftIO $ try @HTTP.HttpException $ HTTP.parseRequest uri >>= flip HTTP.httpLbs httpManager
          req ("http://127.0.0.1:" <> show p0) >>= \case
            Right r | HTTP.responseStatus r == HTTP.ok200 -> exit 0
            e -> if n <= 0
              then errorExit $ "Request to hoogle server failed: " <> T.pack (show e)
              else liftIO (threadDelay (1*10^(6 :: Int))) *> loop (Just $ n - 1)
  where
    verbosity = bool silently verbosely isVerbose
    nixBuild args = run nixBuildPath ("--no-out-link" : args)

    runOb_ = augmentWithVerbosity run_ ob isVerbose
    runOb = augmentWithVerbosity run ob isVerbose
    testObRunInDir' = augmentWithVerbosity testObRunInDir ob isVerbose ["run"]
    testObRunInDirWithArgs' args = augmentWithVerbosity testObRunInDir ob isVerbose (["run"] ++ args)
    testObRunCert' = augmentWithVerbosity testObRunCert ob isVerbose ["run", "-c", "."]
    testObRunInDirWithMissingStaticFile' = augmentWithVerbosity testObRunInDirWithMissingStaticFile ob isVerbose ["run"]
    testObRunInDirWithMissingStaticFilePath' = augmentWithVerbosity testObRunInDirWithMissingStaticFilePath ob isVerbose ["run"]
    testThunkPack' = augmentWithVerbosity testThunkPack ob isVerbose []

    withObeliskImplClean f =
      withSystemTempDirectory "obelisk-impl-clean" $ \obeliskImpl -> do
        void . shellyOb verbosity $ do
          dirtyFiles <- T.strip <$> run gitPath ["-C", toTextIgnore obeliskRepoReadOnly, "diff", "--stat"]
          () <- when (dirtyFiles /= "") $ error "SelfTest does not work correctly with dirty obelisk repos as remote"
          run_ gitPath ["clone", "file://" <> toTextIgnore obeliskRepoReadOnly, toTextIgnore obeliskImpl]
        f obeliskImpl

    withInitCache f =
      withSystemTempDirectory "init Cache λ" $ \initCache -> do
        -- Setup the ob init cache
        void . shellyOb verbosity $ chdir initCache $ do
          runOb_ ["init", "--symlink", toTextIgnore obeliskRepoReadOnly]
          run_ gitPath ["init"]

        f initCache

    shelly_ = void . shellyOb verbosity

    defaultTmpDirName = "test λ"

    inTmp' :: FilePath -> (FilePath -> Sh a) -> IO ()
    inTmp' dirname f = withTmp' dirname (chdir <*> f)
    inTmp = inTmp' defaultTmpDirName

    withTmp' dirname f = shelly_ . withSystemTempDirectory dirname $ f . fromString
    withTmp = withTmp' defaultTmpDirName

    assertRevEQ a b = liftIO . assertEqual "" ""        =<< diff a b
    assertRevNE a b = liftIO . assertBool  "" . (/= "") =<< diff a b

    revParseHead = T.strip <$> run gitPath ["rev-parse", "HEAD"]

    commitAll = do
      run_ gitPath ["add", "."]
      commit "checkpoint"
      revParseHead

    thunk  = ".obelisk/impl"
    update = runOb ["thunk", "update", thunk] *> commitAll
    pack   = runOb ["thunk", "pack",   thunk] *> commitAll
    unpack = runOb ["thunk", "unpack", thunk] *> commitAll

    diff a b = run gitPath ["diff", a, b]


maskExitSuccess :: Sh () -> Sh ()
maskExitSuccess = handle_sh (\case ExitSuccess -> pure (); e -> throw e)

-- | Run `ob run` in the given directory (maximum of one level deep)
testObRunInDir :: String -> [Text] -> Maybe FilePath -> HTTP.Manager -> Sh ()
testObRunInDir executable extraArgs mdir httpManager = maskExitSuccess $ do
  [p0, p1] <- liftIO $ getFreePorts 2
  let uri p = "http://localhost:" <> T.pack (show p) <> "/" -- trailing slash required for comparison
  writefile "config/common/route" $ uri p0
  maybe id chdir mdir $ runHandles executable extraArgs [] $ \_stdin stdout stderr -> do
    firstUri <- handleObRunStdout httpManager stdout stderr
    let newUri = uri p1
    when (firstUri == newUri) $ errorExit $
      "Startup URI (" <> firstUri <> ") is the same as test URI (" <> newUri <> ")"
    maybe id (\_ -> chdir "..") mdir $ alterRouteTo newUri stdout
    runningUri <- handleObRunStdout httpManager stdout stderr
    if runningUri /= newUri
      then errorExit $ "Reloading failed: expected " <> newUri <> " but got " <> runningUri
      else exit 0

testObRunCert :: String -> [Text] -> FilePath -> Maybe FilePath -> Sh ()
testObRunCert executable extraArgs testDir mdir = maskExitSuccess $ do
  -- Generate a TLS key, and then a self-signed certificate using that key
  mapM_ (\cmd -> run_ "nix-shell" ["-p", "openssl", "--command", cmd])
    [ "openssl genrsa -out key.pem 2048"
    , "openssl req -new -key key.pem -out certificate.csr -subj \"/C=US/ST=New York/L=New York/O=Development/OU=IT Department/CN=obsidian.com\""
    , "openssl x509 -req -in certificate.csr -signkey key.pem -out cert.pem"
    ]

  -- One more command is required, for generating the chain.pem file.
  -- For testing purposes, we'll keep it the same as the cert.pem file.
  run_ "cp" ["cert.pem", "chain.pem"]

  -- Also need to change the route inside config/common/route to https
  -- in order to trigger the certificates option (the -c option is not honored for http)
  writefile "config/common/route" "https://localhost:8000"

  maybe id chdir mdir $ runHandles executable extraArgs [] $ \_ stdout _ -> do
    parseObOutput stdout
  where
    parseObOutput h = do
      isEOF <- liftIO $ hIsEOF h
      if isEOF
        then errorExit "Obelisk exited somehow"
        else do
          line <- liftIO $ hGetLineSkipBlanks h
          check line h

    -- Here we check that the certificates are read properly by Obelisk
    -- In case the server is started without reading the certificates, show an error.
    check line h
      | "Using certificate information from:" `T.isPrefixOf` line = do
        -- Should be followed by a `Frontend running on <url>` line
        next <- liftIO $ hGetLineSkipBlanks h
        if "Frontend running on" `T.isPrefixOf` next
          then exit 0
          else errorExit $ "Ran into error: " <> next
      | "Frontend running on" `T.isPrefixOf` line = errorExit "Obelisk did not read the certificates provided via -c option"
      | otherwise = parseObOutput h

-- | Mess up the Cabal file in the given directory, then make sure that
-- @ob run@ fails.
testObRunWithInvalidCabalFile
  :: FilePath  -- ^ Obelisk path
  -> FilePath  -- ^ Directory path (will be made invalid)
  -> Sh ()
testObRunWithInvalidCabalFile executable fp = do
  let commonCabal :: String
      commonCabal = "common/common.cabal"
  liftIO $ writeFile (fp </> commonCabal) "This is not a valid Cabal file."
  errExit False $ do
    runHandles executable ["run"] [] $ \_ _ _ -> pure ()
    lastExitCode >>= \case
      0 -> pure ()
      _ -> errorExit "ob run succeeded even with an invalid Cabal file"

-- | Check whether embedding a non-existent filepath into an obelisk
-- project is detected correctly and generates a compile-time error.
--
-- @'testObRunInDirWithMissingStaticFileWorker' exe args mdir act checkErrorMessage@.
--
-- @checkErrorMessage line1 line2@ is invoked on a compile-time error message of the following form:
--     frontend/src/Frontend.hs:40:32: error:
--        • Static file obelisk.jpg was not found in static.out
--        • In the untyped splice: $(static "obelisk.jpg")
--       |
--    40 |       elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
--       |
--
-- the arguments to @line1@ and @line2@ contain:
--
-- @    • Static file obelisk.jpg was not found in static.out@
--
-- and
--
-- @    • In the untyped splice: $(static "obelisk.jpg")@
--
-- respectively.
testObRunInDirWithMissingStaticFileWorker
  :: String
  -- ^ Path to the @ob@ executable
  -> [Text]
  -- ^ Arguments that are passed to @ob@
  -> Maybe FilePath
  -- ^ Change the working directory of the @ob@ invocation.
  -> Sh ()
  -- ^ Run this action before running @ob run@.
  -- Gives you the opportunity to modify the project to trigger events.
  -- In particular, make sure static files do not exist, s.t. a compilation
  -- error is thrown.
  -> (Text -> Text -> Sh ())
  -- ^ Assertion function for error messages.
  -- Since this function is used for asserting error message contents,
  -- this function specifies what contents two consecutive non-empty lines
  -- are supposed to have.
  -> Sh ()
testObRunInDirWithMissingStaticFileWorker executable extraArgs mdir act checkErrorMessage = maskExitSuccess $ do
  act
  -- Now run `ob run` and read the error
  maybe id chdir mdir $ runHandles executable extraArgs [] $ \_stdin stdout stderr -> do
    parseObOutput stdout
  where
    parseObOutput h = do
      line <- liftIO $ hGetLineSkipBlanks h
      -- This means that everything was successful, this should not be the case
      if line == "Running test..."
        then errorExit "Could not find any error related to static files"
        else checkForErrors line h

    -- As can be seen in the example below, we can parse a lot of data.
    -- For simplicity, we parse the two lines after the `error:` line.
    -- We also ensure that the file name is same, as the file that we changed ie `obelisk.jpg`
    checkForErrors line h
      -- Found an error, check if it is due to static.out
      | "error:" `T.isSuffixOf` line = do
        line1 <- liftIO $ hGetLineSkipBlanks h
        line2 <- liftIO $ hGetLineSkipBlanks h
        checkErrorMessage line1 line2
      | otherwise = parseObOutput h

-- | Run an obelisk test-case, where @static@ is embedding an non-existent
-- filepath.
testObRunInDirWithMissingStaticFile
  :: String
  -- ^ Path to the @ob@ executable
  -> [Text]
  -- ^ Arguments that are passed to @ob@
  -> Maybe FilePath
  -- ^ Change the working directory of the @ob@ invocation.
  -> Sh ()
testObRunInDirWithMissingStaticFile executable extraArgs mdir =
  testObRunInDirWithMissingStaticFileWorker executable extraArgs mdir act checkStaticError
  where
    -- | Rename a static file, so that `ob run` will fail with a specific error
    act = run_ mvPath ["static/obelisk.jpg", "static/obelisk2.jpg"]
    -- We are looking for an error of the following type:
    --  frontend/src/Frontend.hs:40:32: error:
    --     • Static file obelisk.jpg was not found in static.out
    --     • In the untyped splice: $(static "obelisk.jpg")
    --    |
    -- 40 |       elAttr "img" ("src" =: $(static "obelisk.jpg")) blank
    --    |                                ^^^^^^^^^^^^^^^^^^^^
    checkStaticError line1 line2 =
      if name1 == Just "obelisk.jpg" && name2 == Just "obelisk.jpg"
        then exit 0
        else errorExit $ "Expected a different error:\n" <> line1 <> "\n" <> line2
      where
        name1 = T.stripPrefix "• The file " (T.stripStart line1) >>= T.stripSuffix " was not found in static.out"
        name2 = T.stripPrefix "• In the untyped splice: $(static \"" (T.stripStart line2) >>= T.stripSuffix "\")"

-- | Run an obelisk test-case, where @staticFilePath@ is embedding an
-- non-existent filepath.
testObRunInDirWithMissingStaticFilePath
  :: String
  -- ^ Path to the @ob@ executable
  -> [Text]
  -- ^ Arguments that are passed to @ob@
  -> Maybe FilePath
  -- ^ Change the working directory of the @ob@ invocation.
  -> Sh ()
testObRunInDirWithMissingStaticFilePath executable extraArgs mdir =
  testObRunInDirWithMissingStaticFileWorker executable extraArgs mdir act checkStaticError
  where
    -- | Rename a static file, so that `ob run` will fail with a specific error
    act = do
      run_ sedPath ["-e", "s/\\$(static /\\$(staticFilePath /", "-i", "frontend/src/Frontend.hs"]
      run_ mvPath ["static/obelisk.jpg", "static/obelisk2.jpg"]
    -- We are looking for an error of the following type:
    --  frontend/src/Frontend.hs:40:32: error:
    --     • Static file obelisk.jpg was not found in static.out
    --     • In the untyped splice: $(staticFilePath "obelisk.jpg")
    --    |
    -- 40 |       elAttr "img" ("src" =: $(staticFilePath "obelisk.jpg")) blank
    --    |                                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    checkStaticError line1 line2 =
      if name1 == Just "obelisk.jpg" && name2 == Just "obelisk.jpg"
        then exit 0
        else errorExit $ "Expected a different error:\n" <> line1 <> "\n" <> line2
      where
        name1 = T.stripPrefix "• The file " (T.stripStart line1) >>= T.stripSuffix " was not found in static.out"
        name2 = T.stripPrefix "• In the untyped splice: $(staticFilePath \"" (T.stripStart line2) >>= T.stripSuffix "\")"

testThunkPack :: String -> [Text] -> FilePath -> Sh ()
testThunkPack executable args path' = withTempFile repoDir "test-file" $ \file handle -> do
  let
    pack' = readProcessWithExitCode executable (["thunk", "pack", path'] ++ map T.unpack args) ""
    ensureThunkPackFails q = liftIO $ pack' >>= \case
      (code, out, err)
        | code == ExitSuccess -> fail $ "ob thunk pack succeeded when it should have failed with error '" <> show q <> "'"
        | q `T.isInfixOf` T.pack (out <> err) -> pure ()
        | otherwise -> fail $ "ob thunk pack failed for an unexpected reason, expecting '" <> show q <> "', received: " <> show out <> "\nstderr: " <> err
    git = chdir repoDir . run_ gitPath
  -- Untracked files
  ensureThunkPackFails "Untracked files"
  git ["add", T.pack file]
  -- Uncommitted files (staged)
  ensureThunkPackFails "unsaved"
  chdir repoDir $ commit "test commit"
  -- Non-pushed commits in any branch
  ensureThunkPackFails "not yet pushed"
  -- Uncommitted files (unstaged)
  liftIO $ T.hPutStrLn handle "test file" >> hClose handle
  ensureThunkPackFails "modified"
  -- Existing stashes
  git $ gitUserConfig <> [ "stash" ]
  ensureThunkPackFails "has stashes"
  where
    repoDir = path' </> unpackedDirName

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
handleObRunStdout :: HTTP.Manager -> Handle -> Handle -> Sh Text
handleObRunStdout httpManager stdout stderr = flip fix (ObRunState_Init, []) $ \loop (state, msgs) -> do
  isEOF <- liftIO $ hIsEOF stdout
  if isEOF
  then handleObRunError msgs
  else liftIO (T.hGetLine stdout) >>= \t -> case state of
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
    _ | "Failed" `T.isPrefixOf` t -> handleObRunError (t : msgs)
      | otherwise -> loop (state, t : msgs)
  where
    handleObRunError msgs = do
      stderrContent <- liftIO $ hGetContents stderr
      errorExit $ "ob run failed: " <> T.unlines (reverse msgs) <> " stderr: " <> T.pack stderrContent

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

testLegacyGitThunks :: Bool -> IO ()
testLegacyGitThunks isVerbose = withSystemTempDirectory "test-git-repo" $ \gitDir -> do
  isolatedGitShell isVerbose gitDir $ \git -> do
    writefile (gitDir </> ("default.nix" :: FilePath)) "{}: \"hello\""
    _ <- git ["add", "--all"]
    _ <- git ["commit", "-m", "Initial commit"]
    rev <- T.strip <$> git ["rev-parse", "HEAD"]
    sha256 :: Text
      <-  either error pure
      =<< (Aeson.parseEither (Aeson..: "sha256") <=< Aeson.eitherDecodeStrict . T.encodeUtf8)
      <$> run nixPrefetchGitPath [toTextIgnore gitDir]

    for_ (legacyGitThunks (GitThunkParams gitDir rev sha256)) $ \mkFiles ->
      withSystemTempDirectory "test-thunks" $ \thunkDir -> do
        liftIO $ mkFiles thunkDir
        run_ "ob" ["thunk", "unpack", toTextIgnore thunkDir]
        run_ "ob" ["thunk", "pack", toTextIgnore thunkDir]

data GitThunkParams = GitThunkParams
  { _gitThunkParams_repo :: !FilePath
  , _gitThunkParams_rev :: !Text
  , _gitThunkParams_sha256 :: !Text
  } deriving Show

legacyGitThunks :: GitThunkParams -> [FilePath -> IO ()]
legacyGitThunks (GitThunkParams repo' rev sha256) =
  [ mkLegacyIO
      (T.unlines
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "let fetchGit = {url, rev, ref ? null, branch ? null, sha256 ? null, fetchSubmodules ? null}:"
        , "  assert !fetchSubmodules; (import <nixpkgs> {}).fetchgit { inherit url rev sha256; };"
        , "in import (fetchGit (builtins.fromJSON (builtins.readFile ./git.json)))"
        ]
      )
      (Map.fromList
        [ ("url", repo)
        , ("rev", rev)
        , ("branch", "master")
        , ("sha256", sha256)
        ]
      )
  , mkLegacyIO
      (T.unlines
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "let fetchGit = {url, rev, ref ? null, branch ? null, sha256 ? null, fetchSubmodules ? null}:"
        , "  if builtins.hasAttr \"fetchGit\" builtins"
        , "    then builtins.fetchGit ({ inherit url rev; } // (if branch == null then {} else { ref = branch; }))"
        , "    else abort \"Plain Git repositories are only supported on nix 2.0 or higher.\";"
        , "in import (fetchGit (builtins.fromJSON (builtins.readFile ./git.json)))"
        ]
      )
      (Map.fromList
        [ ("url", repo)
        , ("rev", rev)
        , ("branch", "master")
        , ("sha256", sha256)
        ]
      )
  , mkLegacyIO
      (T.unlines
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "let fetch = {url, rev, ref ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:"
        , "  let realUrl = let firstChar = builtins.substring 0 1 url; in"
        , "    if firstChar == \"/\" then /. + url"
        , "    else if firstChar == \".\" then ./. + url"
        , "    else url;"
        , "  in if !fetchSubmodules && private then builtins.fetchGit {"
        , "    url = realUrl; inherit rev;"
        , "  } else (import <nixpkgs> {}).fetchgit {"
        , "    url = realUrl; inherit rev sha256;"
        , "  };"
        , "in import (fetch (builtins.fromJSON (builtins.readFile ./git.json)))"
        ]
      )
      (Map.fromList
        [ ("url", Aeson.String repo)
        , ("rev", Aeson.String rev)
        , ("branch", Aeson.String "master")
        , ("sha256", Aeson.String sha256)
        , ("private", Aeson.Bool False)
        ]
      )
  , mkLegacyIO
      (T.unlines
        [ "# DO NOT HAND-EDIT THIS FILE"
        , "let fetch = {url, rev, branch ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:"
        , "  let realUrl = let firstChar = builtins.substring 0 1 url; in"
        , "    if firstChar == \"/\" then /. + url"
        , "    else if firstChar == \".\" then ./. + url"
        , "    else url;"
        , "  in if !fetchSubmodules && private then builtins.fetchGit {"
        , "    url = realUrl; inherit rev;"
        , "    ${if branch == null then null else \"ref\"} = branch;"
        , "  } else (import <nixpkgs> {}).fetchgit {"
        , "    url = realUrl; inherit rev sha256;"
        , "  };"
        , "in import (fetch (builtins.fromJSON (builtins.readFile ./git.json)))"
        ]
      )
      (Map.fromList
        [ ("url", Aeson.String repo)
        , ("rev", Aeson.String rev)
        , ("branch", Aeson.String "master")
        , ("sha256", Aeson.String sha256)
        , ("private", Aeson.Bool False)
        ]
      )
  ]
  where
    repo = T.pack repo'
    mkLegacyIO :: Aeson.ToJSON v => Text -> Map.Map Text v -> FilePath -> IO ()
    mkLegacyIO defaultNix gitJson dir = do
      T.writeFile (dir </> ("default.nix" :: FilePath)) defaultNix
      LBS.writeFile (dir </> ("git.json" :: FilePath)) (Aeson.encode gitJson)

unpackedDirName :: FilePath
unpackedDirName = "."

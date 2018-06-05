{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Binary as Binary
import Data.Bool (bool)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe (catMaybes, listToMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.StaticPtr
import Options.Applicative
import System.Directory
import System.Environment
import System.FilePath
import qualified System.Info
import System.IO (hIsTerminalDevice, stdout)
import System.Posix.Process (executeFile)

import Obelisk.App
import Obelisk.CliApp
import Obelisk.CliApp.Demo (cliDemo)
import Obelisk.Command.Deploy
import Obelisk.Command.Project
import Obelisk.Command.Run
import Obelisk.Command.Thunk
import Obelisk.Command.Upgrade (decideHandOffToProjectOb, migrateObelisk, upgradeObelisk)
import qualified Obelisk.Command.VmBuilder as VmBuilder
import Obelisk.Migration (Hash)


data Args = Args
  { _args_noHandOffPassed :: Bool
  -- ^ This flag is actually handled outside of the optparse-applicative parser, but we detect whether
  -- it has gotten through in order to notify the user that it should only be passed once and as the very
  -- first argument
  , _args_verbose :: Bool
  , _args_command :: ObCommand
  }
  deriving Show

newtype ArgsConfig = ArgsConfig
  { _argsConfig_enableVmBuilderByDefault :: Bool
  }

args :: ArgsConfig -> Parser Args
args cfg = Args <$> noHandoff <*> verbose <*> obCommand cfg

noHandoff :: Parser Bool
noHandoff = flag False True $ mconcat
  [ long "no-handoff"
  , help "Do not hand off execution to project-specific implementation of this command"
  , hidden
  ]

verbose :: Parser Bool
verbose = flag False True $ mconcat
  [ long "verbose"
  , short 'v'
  , help "Be more verbose"
  ]

argsInfo :: ArgsConfig -> ParserInfo Args
argsInfo cfg = info (args cfg <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Manage Obelisk projects"
  ]

initSource :: Parser InitSource
initSource = foldl1 (<|>)
  [ pure InitSource_Default
  , InitSource_Branch <$> strOption (long "branch" <> metavar "BRANCH")
  , InitSource_Symlink <$> strOption (long "symlink" <> action "directory" <> metavar "PATH")
  ]

data ObCommand
   = ObCommand_Init InitSource
   | ObCommand_Deploy DeployCommand
   | ObCommand_Run
   | ObCommand_Thunk ThunkCommand
   | ObCommand_Repl
   | ObCommand_Upgrade Text
   | ObCommand_Internal ObInternal
   deriving Show

data ObInternal
   = ObInternal_RunStaticIO StaticKey
   | ObInternal_Migrate Hash
   | ObInternal_CLIDemo
   deriving Show

inNixShell' :: MonadObelisk m => StaticPtr (ObeliskT IO ()) -> m ()
inNixShell' p = withProjectRoot "." $ \root -> do
  cmd <- liftIO $ unwords <$> mkCmd  -- TODO: shell escape instead of unwords
  projectShell root False "ghc" cmd
  where
    mkCmd = do
      argsCfg <- getArgsConfig
      myArgs <- getArgs
      obArgs <- parseCLIArgs argsCfg myArgs
      progName <- getExecutablePath
      return $ progName : catMaybes
        [ Just "--no-handoff"
        , bool Nothing (Just "--verbose") $ _args_verbose obArgs
        , Just "internal"
        , Just "run-static-io"
        , Just $ encodeStaticKey $ staticKey p
        ]

obCommand :: ArgsConfig -> Parser ObCommand
obCommand cfg = hsubparser
    (mconcat
      [ command "init" $ info (ObCommand_Init <$> initSource) $ progDesc "Initialize an Obelisk project"
      , command "deploy" $ info (ObCommand_Deploy <$> deployCommand cfg) $ progDesc "Prepare a deployment for an Obelisk project"
      , command "run" $ info (pure ObCommand_Run) $ progDesc "Run current project in development mode"
      , command "thunk" $ info (ObCommand_Thunk <$> thunkCommand) $ progDesc "Manipulate thunk directories"
      , command "repl" $ info (pure ObCommand_Repl) $ progDesc "Open an interactive interpreter"
      , command "upgrade" $ info (ObCommand_Upgrade <$> strArgument (action "branch" <> metavar "GITBRANCH" <> help "Git branch of obelisk to update to")) $ progDesc "Upgrade Obelisk in the project"
      ])
  <|> subparser
    (mconcat
      [ internal
      , command "internal" (info (ObCommand_Internal <$> internalCommand) mempty)
      ])

deployCommand :: ArgsConfig -> Parser DeployCommand
deployCommand cfg = hsubparser $ mconcat
  [ command "init" $ info (DeployCommand_Init <$> deployInitOpts) $ progDesc "Initialize a deployment configuration directory"
  , command "push" $ info (DeployCommand_Push <$> remoteBuilderParser) mempty
  , command "test" $ info (DeployCommand_Test <$> platformP) $ progDesc "Test your obelisk project from a mobile platform."
  , command "update" $ info (pure DeployCommand_Update) $ progDesc "Update the deployment's src thunk to latest"
  ]
  where
    platformP = hsubparser $ mconcat
      [ command "android" $ info (pure Android) mempty
      , command "ios"     $ info (pure IOS <*> strArgument (metavar "TEAMID" <> help "Your Team ID - found in the Apple developer portal")) mempty
      ]

    remoteBuilderParser :: Parser (Maybe RemoteBuilder)
    remoteBuilderParser =
      flag (if enabledByDefault then enabled else Nothing) enabled (mconcat
        [ long $ "enable-" <> flagBase
        , help $ "Enable " <> flagDesc <> (if enabledByDefault then " (default)" else "")
        ])
      <|> flag enabled Nothing (mconcat
        [ long $ "disable-" <> flagBase
        , help $ "Disable a " <> flagDesc <> (if not enabledByDefault then " (default)" else "")
        ])
      where
        enabledByDefault = _argsConfig_enableVmBuilderByDefault cfg
        enabled = Just RemoteBuilder_ObeliskVM
        flagBase = "vm-builder"
        flagDesc = "managed Linux virtual machine as a Nix remote builder (requires Docker)"


deployInitOpts :: Parser DeployInitOpts
deployInitOpts = DeployInitOpts
  <$> strArgument (action "directory" <> metavar "DEPLOYDIR" <> help "Path to a directory that it will create")
  <*> strOption (long "ssh-key" <> action "file" <> metavar "SSHKEY" <> help "Path to an ssh key that it will symlink to")
  <*> some (strOption (long "hostname" <> metavar "HOSTNAME" <> help "hostname of the deployment target"))
  <*> strOption (long "upstream" <> value "origin" <> metavar "REMOTE" <> help "git remote to use for the src thunk" <> showDefault)
  <*> sslOpts
  where
    sslOpts :: Parser SslConfigOpts
    sslOpts = SslConfigOpts
      <$> strOption (long "ssl-hostname" <> metavar "SSLHOSTNAME" <> help "SSL hostname")
      <*> strOption (long "ssl-email" <> metavar "SSLEMAIL" <> help "SSL admin email")

type TeamID = String
data PlatformDeployment = Android | IOS TeamID
  deriving (Show)

data RemoteBuilder = RemoteBuilder_ObeliskVM
  deriving (Eq, Show)

data DeployCommand
  = DeployCommand_Init DeployInitOpts
  | DeployCommand_Push (Maybe RemoteBuilder)
  | DeployCommand_Test PlatformDeployment
  | DeployCommand_Update
  deriving Show

data SslConfigOpts = SslConfigOpts
  { _sslConfigOpts_hostname :: String
  , _sslConfigOpts_adminEmail :: String
  }
  deriving Show

data DeployInitOpts = DeployInitOpts
  { _deployInitOpts_outputDir :: FilePath
  , _deployInitOpts_sshKey :: FilePath
  , _deployInitOpts_hostname :: [String]
  , _deployInitOpts_remote :: String
  , _deployInitOpts_sslConfig :: SslConfigOpts
  }
  deriving Show

internalCommand :: Parser ObInternal
internalCommand = subparser $ mconcat
  [ command "run-static-io" $ info (ObInternal_RunStaticIO <$> argument (eitherReader decodeStaticKey) (action "static-key")) mempty
  , command "migrate" $ info (ObInternal_Migrate <$> strArgument (action "fromHash" <> metavar "FROMHASH" <> help "Migrate from this hash")) $ progDesc "Perform a migrate from the given hash to HEAD of obelisk thunk"
  , command "clidemo" $ info (pure ObInternal_CLIDemo) mempty
  ]

--TODO: Result should provide normalised path and also original user input for error reporting.
thunkDirectoryParser :: Parser FilePath
thunkDirectoryParser = fmap (dropTrailingPathSeparator . normalise) . strArgument $ mconcat
  [ action "directory"
  , metavar "THUNKDIR"
  , help "Path to directory containing thunk data"
  ]

data ThunkPackOpts = ThunkPackOpts
  { _thunkPackOpts_directory :: FilePath
  , _thunkPackOpts_upstream :: String
  }
  deriving Show

data ThunkCommand
   = ThunkCommand_Update [FilePath]
   | ThunkCommand_Unpack [FilePath]
   | ThunkCommand_Pack [ThunkPackOpts]
  deriving Show

thunkPackOpts :: Parser ThunkPackOpts
thunkPackOpts = (ThunkPackOpts <$> thunkDirectoryParser <*>) . strOption $ mconcat
  [ long "upstream"
  , value "origin"
  , metavar "REMOTE"
  , help "Git remote that packed thunk will point to"
  , showDefault
  ]

thunkCommand :: Parser ThunkCommand
thunkCommand = hsubparser $ mconcat
  [ command "update" $ info (ThunkCommand_Update <$> some thunkDirectoryParser) $ progDesc "Update thunk to latest revision available"
  , command "unpack" $ info (ThunkCommand_Unpack <$> some thunkDirectoryParser) $ progDesc "Unpack thunk into git checkout of revision it points to"
  , command "pack" $ info (ThunkCommand_Pack <$> some thunkPackOpts) $ progDesc "Pack git checkout into thunk that points at given upstream"
  ]

parserPrefs :: ParserPrefs
parserPrefs = defaultPrefs
  { prefShowHelpOnEmpty = True
  }

parseCLIArgs :: ArgsConfig -> [String] -> IO Args
parseCLIArgs cfg as = pure as >>= handleParseResult . execParserPure parserPrefs (argsInfo cfg)

mkObeliskConfig :: IO Obelisk
mkObeliskConfig = do
  cliArgs <- getArgs
  -- This function should not use argument parser (full argument parsing happens post handoff)
  let logLevel = toLogLevel $ "-v" `elem` cliArgs
  notInteractive <- not <$> isInteractiveTerm
  cliConf <- newCliConfig logLevel notInteractive notInteractive
  return $ Obelisk cliConf
  where
    toLogLevel = bool Notice Debug
    isInteractiveTerm = do
      isTerm <- hIsTerminalDevice stdout
      -- Running in bash/fish/zsh completion
      inShellCompletion <- liftIO $ isInfixOf "completion" . unwords <$> getArgs
      return $ isTerm && not inShellCompletion

main :: IO ()
main = do
  argsCfg <- getArgsConfig
  mkObeliskConfig >>= (`runObelisk` ObeliskT (main' argsCfg))

main' :: MonadObelisk m => ArgsConfig -> m ()
main' argsCfg = do
  obPath <- liftIO getExecutablePath
  myArgs <- liftIO getArgs
  logLevel <- getLogLevel
  putLog Debug $ T.pack $ unwords
    [ "Starting Obelisk <" <> obPath <> ">"
    , "args=" <> show myArgs
    , "logging-level=" <> show logLevel
    ]

  (mainWithHandOff argsCfg <=< parseHandoff) =<< liftIO getArgs

-- Type representing the result of a handoff calculation.
data HandOff m
  = HandOff_Yes FilePath [String] -- Handoff immediately to the given impl with the given args
  | HandOff_Decide (m Bool) FilePath [String]  -- Handoff only if the action returns True
  | HandOff_No [String]  -- Do not handoff, and continue with the given args

mainWithHandOff :: MonadObelisk m => ArgsConfig -> HandOff m -> m ()
mainWithHandOff argsCfg = \case
  HandOff_Yes impl as -> do
    putLog Debug $ "Handing off to project obelisk " <> T.pack impl
    liftIO $ executeFile impl False ("--no-handoff" : as) Nothing
  HandOff_Decide f impl as -> do
    withSpinner' "Deciding whether to handoff"
      (Just $ bool
        "Decided /not/ to handoff to project obelisk"
        "Decided to handoff to project obelisk") f >>= \case
      True -> mainWithHandOff argsCfg $ HandOff_Yes impl as
      False -> mainWithHandOff argsCfg $ HandOff_No as
  HandOff_No as -> do
    putLog Debug $ "Not handing off"
    args' <- liftIO $ parseCLIArgs argsCfg as
    warnIfExtraneousFlag args'
    ob $ _args_command args'
  where
    warnIfExtraneousFlag a = case _args_noHandOffPassed a of
      False -> return ()
      True -> putLog Warning $
        "Ignoring unexpected --no-handoff (should only be passed once and as the first argument)"

-- Handoff logic
parseHandoff :: MonadObelisk m => [String] -> m (HandOff m)
parseHandoff as' = case hasNoHandoff as' of
  (True, as) ->
    pure $ HandOff_No as
  (False, as) -> findProjectObeliskCommand "." >>= \case
    Nothing -> do
      putLog Debug "Not in a project; no need to hand off"
      pure $ HandOff_No as
    Just impl -> case getSubCommand as of
      Just "upgrade" ->
        pure $ HandOff_Decide (decideHandOffToProjectOb ".") impl as
      _ ->
        pure $ HandOff_Yes impl as
  where
    getSubCommand = listToMaybe . filter (not . isPrefixOf "-")
    hasNoHandoff = \case
      "--no-handoff" : xs ->
        -- If we've been told not to hand off, don't hand off
        (True, xs)
      x:xs
        -- Otherwise bash completion would always hand-off even if the user isn't trying to
        | "--bash-completion" `isPrefixOf` x && "--no-handoff" `elem` xs ->
          (True, x:xs)
        | otherwise ->
          (False, x:xs)
      xs ->
        (False, xs)

ob :: MonadObelisk m => ObCommand -> m ()
ob = \case
  ObCommand_Init source -> initProject source
  ObCommand_Deploy dc -> case dc of
    DeployCommand_Init deployOpts -> withProjectRoot "." $ \root -> do
      let deployDir = _deployInitOpts_outputDir deployOpts
      r <- liftIO $ canonicalizePath root
      rootEqualsTarget <- liftIO $ equalFilePath r <$> canonicalizePath deployDir
      when rootEqualsTarget $
        failWith $ "Deploy directory " <> T.pack deployDir <> " should not be the same as project root."
      thunkPtr <- readThunk root >>= \case
        Left err -> failWith $ case err of
          ReadThunkError_AmbiguousFiles ->
            "Project root " <> T.pack r <> " is not a git repository or valid thunk"
          ReadThunkError_UnrecognizedFiles ->
            "Project root " <> T.pack r <> " is not a git repository or valid thunk"
          _ -> "thunk read: " <> T.pack (show err)
        Right (ThunkData_Packed ptr) -> return ptr
        Right (ThunkData_Checkout (Just ptr)) -> return ptr
        Right (ThunkData_Checkout Nothing) ->
          getThunkPtr' False root (T.pack $ _deployInitOpts_remote deployOpts)
      let sshKeyPath = _deployInitOpts_sshKey deployOpts
          hostname = _deployInitOpts_hostname deployOpts
          sslHostname = _sslConfigOpts_hostname $
            _deployInitOpts_sslConfig deployOpts
          sslEmail = _sslConfigOpts_adminEmail $
            _deployInitOpts_sslConfig deployOpts
      deployInit thunkPtr (root </> "config") deployDir sshKeyPath hostname sslHostname sslEmail
    DeployCommand_Push remoteBuilder -> deployPush "." $ case remoteBuilder of
      Nothing -> pure []
      Just RemoteBuilder_ObeliskVM -> (:[]) <$> VmBuilder.getNixBuildersArg
    DeployCommand_Update -> deployUpdate "."
    DeployCommand_Test Android -> deployMobile "android" []
    DeployCommand_Test (IOS teamID) -> deployMobile "ios" [teamID]
  ObCommand_Run -> inNixShell' $ static run
    -- inNixShell ($(mkClosure 'ghcidAction) ())
  ObCommand_Thunk tc -> case tc of
    ThunkCommand_Update thunks -> mapM_ updateThunkToLatest thunks
    ThunkCommand_Unpack thunks -> mapM_ unpackThunk thunks
    ThunkCommand_Pack thunks -> forM_ thunks $ \(ThunkPackOpts dir upstream) -> packThunk dir (T.pack upstream)
  ObCommand_Repl -> runRepl
  ObCommand_Upgrade branch -> do
    upgradeObelisk "." branch
  ObCommand_Internal icmd -> case icmd of
    ObInternal_RunStaticIO k -> liftIO (unsafeLookupStaticPtr @(ObeliskT IO ()) k) >>= \case
      Nothing -> failWith $ "ObInternal_RunStaticIO: no such StaticKey: " <> T.pack (show k)
      Just p -> do
        c <- getObelisk
        liftIO $ runObelisk c $ deRefStaticPtr p
    ObInternal_Migrate fromHash -> do
      migrateObelisk "." fromHash
    ObInternal_CLIDemo -> cliDemo

--TODO: Clean up all the magic strings throughout this codebase

getArgsConfig :: IO ArgsConfig
getArgsConfig = pure $ ArgsConfig { _argsConfig_enableVmBuilderByDefault = System.Info.os == "darwin" }

encodeStaticKey :: StaticKey -> String
encodeStaticKey = T.unpack . decodeUtf8 . Base16.encode . LBS.toStrict . Binary.encode

-- TODO: Use failWith in place of fail to be consistent.
decodeStaticKey :: String -> Either String StaticKey
decodeStaticKey s = case Base16.decode $ encodeUtf8 $ T.pack s of
  (b, "") -> case Binary.decodeOrFail $ LBS.fromStrict b of
    Right ("", _, a) -> pure a
    Right _ -> fail "decodeStaticKey: Binary.decodeOrFail didn't consume all input"
    Left (_, _, e) -> fail $ "decodeStaticKey: Binary.decodeOrFail failed: " <> show e
  _ -> fail $ "decodeStaticKey: could not decode hex string: " <> show s

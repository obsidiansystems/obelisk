{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command where

import Control.Concurrent (newMVar)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import qualified Data.Binary as Binary
import Data.Bool (bool)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.StaticPtr
import Options.Applicative
import System.Environment
import System.FilePath
import System.IO (hIsTerminalDevice, stdout)
import System.Posix.Process (executeFile)

import Obelisk.App
import Obelisk.CLI (LoggingConfig (LoggingConfig, _loggingConfig_level), Severity (..), cliDemo, failWith,
                    putLog)
import Obelisk.Command.Deploy
import Obelisk.Command.Project
import Obelisk.Command.Repl
import Obelisk.Command.Run
import Obelisk.Command.Thunk

data Args = Args
  { _args_noHandOffPassed :: Bool
  -- ^ This flag is actually handled outside of the optparse-applicative parser, but we detect whether
  -- it has gotten through in order to notify the user that it should only be passed once and as the very
  -- first argument
  , _args_verbose :: Bool
  , _args_command :: ObCommand
  }
  deriving Show

args :: Parser Args
args = Args <$> noHandoff <*> verbose <*> obCommand

noHandoff :: Parser Bool
noHandoff = flag False True $ mconcat
  [ long "no-handoff"
  , help "Do not hand off execution to project-specific implementation of this command"
  , hidden
  ]

verbose :: Parser Bool
verbose = flag False True $ mconcat
  [ long "verbose"
  , help "Be more verbose"
  ]

argsInfo :: ParserInfo Args
argsInfo = info (args <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Manage Obelisk projects"
  ]

initSource :: Parser InitSource
initSource = foldl1 (<|>)
  [ pure InitSource_Default
  , InitSource_Branch <$> strOption (long "branch" <> metavar "BRANCH")
  , InitSource_Symlink <$> strOption (long "symlink" <> metavar "PATH")
  ]

data ObCommand
   = ObCommand_Init InitSource
   | ObCommand_Deploy DeployCommand
   | ObCommand_Run
   | ObCommand_Thunk ThunkCommand
   | ObCommand_Repl FilePath
   | ObCommand_Watch FilePath
   | ObCommand_Internal ObInternal
   deriving Show

data ObInternal
   = ObInternal_RunStaticIO StaticKey
   | ObInternal_CLIDemo
   deriving Show

inNixShell' :: MonadObelisk m => StaticPtr (IO ()) -> m ()
inNixShell' p = withProjectRoot "." $ \root -> do
  cmd <- liftIO $ unwords <$> mkCmd  -- TODO: shell escape instead of unwords
  projectShell root False "ghc" cmd
  where
    mkCmd = do
      obArgs <- getObArgs
      progName <- getExecutablePath
      return $ progName : catMaybes
        [ Just "--no-handoff"
        , bool Nothing (Just "--verbose") $ _args_verbose obArgs
        , Just "internal"
        , Just "run-static-io"
        , Just $ encodeStaticKey $ staticKey p
        ]

obCommand :: Parser ObCommand
obCommand = hsubparser
    (mconcat
      [ command "init" $ info (ObCommand_Init <$> initSource) $ progDesc "Initialize an Obelisk project"
      , command "deploy" $ info (ObCommand_Deploy <$> deployCommand) $ progDesc "Prepare a deployment for an Obelisk project"
      , command "run" $ info (pure ObCommand_Run) $ progDesc "Run current project in development mode"
      , command "thunk" $ info (ObCommand_Thunk <$> thunkCommand) $ progDesc "Manipulate thunk directories"
      , command "repl" $ info (ObCommand_Repl <$> strArgument (action "directory")) $ progDesc "Open an interactive interpreter"
      , command "watch" $ info (ObCommand_Watch <$> strArgument (action "directory"))$ progDesc "Watch directory for changes and update interactive interpreter"
      ])
  <|> subparser
    (mconcat
      [ internal
      , command "internal" (info (ObCommand_Internal <$> internalCommand) mempty)
      ])

deployCommand :: Parser DeployCommand
deployCommand = hsubparser $ mconcat
  [ command "init" $ info deployInitCommand $ progDesc "Initialize a deployment configuration directory"
  , command "push" $ info (pure DeployCommand_Push) mempty
  , command "update" $ info (pure DeployCommand_Update) $ progDesc "Update the deployment's src thunk to latest"
  ]

deployInitCommand :: Parser DeployCommand
deployInitCommand = fmap DeployCommand_Init $ DeployInitOpts
  <$> strArgument (action "directory" <> metavar "DEPLOYDIR" <> help "Path to a directory that it will create")
  <*> strOption (long "ssh-key" <> action "file" <> metavar "SSHKEY" <> help "Path to an ssh key that it will symlink to")
  <*> some (strOption (long "hostname" <> metavar "HOSTNAME" <> help "hostname of the deployment target"))
  <*> strOption (long "upstream" <> value "origin" <> metavar "REMOTE" <> help "git remote to use for the src thunk" <> showDefault)

data DeployCommand
  = DeployCommand_Init DeployInitOpts
  | DeployCommand_Push
  | DeployCommand_Update
  deriving Show

data DeployInitOpts = DeployInitOpts
  { _deployInitOpts_ouputDir :: FilePath
  , _deployInitOpts_sshKey :: FilePath
  , _deployInitOpts_hostname :: [String]
  , _deployInitOpts_remote :: String
  }
  deriving Show

internalCommand :: Parser ObInternal
internalCommand = subparser $ mconcat
  [ command "run-static-io" $ info (ObInternal_RunStaticIO <$> argument (eitherReader decodeStaticKey) (action "static-key")) mempty
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

-- | Are we on a terminal with active user interaction?
isInteractiveTerm :: IO Bool
isInteractiveTerm = do
  isTerm <- hIsTerminalDevice stdout
  -- Running in bash/fish/zsh completion
  inShellCompletion <- liftIO $ isInfixOf "completion" . unwords <$> getArgs
  return $ isTerm && not inShellCompletion

mkObeliskConfig :: IO Obelisk
mkObeliskConfig = do
  logLevel <- getLogLevel <$> getObArgs
  notInteractive <- not <$> isInteractiveTerm
  loggingConfig <- LoggingConfig logLevel notInteractive <$> newMVar False
  return $ Obelisk notInteractive loggingConfig
  where
    getLogLevel = bool Notice Debug . _args_verbose

getObArgs :: IO Args
getObArgs = getArgs >>= handleParseResult . execParserPure parserPrefs argsInfo

main :: IO ()
main = mkObeliskConfig >>= (`runObelisk` ObeliskT main')

main' :: MonadObelisk m => m ()
main' = do
  c <- ask
  obPath <- liftIO getExecutablePath
  obArgs <- liftIO getObArgs
  putLog Debug $ T.pack $ unwords
    [ "Starting Obelisk <" <> obPath <> ">"
    , "args=" <> show obArgs
    , "noSpinner=" <> show (_obelisk_noSpinner c)
    , "logging-level=" <> show (_loggingConfig_level $ _obelisk_logging c)
    ]

  --TODO: We'd like to actually use the parser to determine whether to hand off,
  --but in the case where this implementation of 'ob' doesn't support all
  --arguments being passed along, this could fail.  For now, we don't bother
  --with optparse-applicative until we've done the handoff.
  myArgs <- liftIO getArgs
  let go as = do
        args' <- liftIO $ handleParseResult (execParserPure parserPrefs argsInfo as)
        case _args_noHandOffPassed args' of
          False -> return ()
          True -> putLog Warning "--no-handoff should only be passed once and as the first argument; ignoring"
        ob $ _args_command args'
      handoffAndGo as = findProjectObeliskCommand "." >>= \case
        Nothing -> go as -- If not in a project, just run ourselves
        Just impl -> do
          -- Invoke the real implementation, using --no-handoff to prevent infinite recursion
          putLog Debug $ "Handing off to " <> T.pack impl
          liftIO $ executeFile impl False ("--no-handoff" : myArgs) Nothing
  case myArgs of
    "--no-handoff" : as -> go as -- If we've been told not to hand off, don't hand off
    a:as -- Otherwise bash completion would always hand-off even if the user isn't trying to
      | "--bash-completion" `isPrefixOf` a
      && "--no-handoff" `elem` as -> go (a:as)
      | otherwise -> handoffAndGo (a:as)
    as -> handoffAndGo as

ob :: MonadObelisk m => ObCommand -> m ()
ob = \case
  ObCommand_Init source -> initProject source
  ObCommand_Deploy dc -> case dc of
    DeployCommand_Init deployOpts -> withProjectRoot "." $ \root -> do
      thunkPtr <- readThunk root >>= \case
        Left err -> failWith $ T.pack $ "thunk pack: " <> show err
        Right (ThunkData_Packed ptr) -> return ptr
        Right (ThunkData_Checkout (Just ptr)) -> return ptr
        Right (ThunkData_Checkout Nothing) ->
          getThunkPtr root (_deployInitOpts_remote deployOpts)
      let deployDir = _deployInitOpts_ouputDir deployOpts
          sshKeyPath = _deployInitOpts_sshKey deployOpts
          hostname = _deployInitOpts_hostname deployOpts
      deployInit thunkPtr (root </> "config") deployDir sshKeyPath hostname
    DeployCommand_Push -> deployPush "."
    DeployCommand_Update -> deployUpdate "."
  ObCommand_Run -> inNixShell' $ static run
    -- inNixShell ($(mkClosure 'ghcidAction) ())
  ObCommand_Thunk tc -> case tc of
    ThunkCommand_Update thunks -> mapM_ updateThunkToLatest thunks
    ThunkCommand_Unpack thunks -> mapM_ unpackThunk thunks
    ThunkCommand_Pack thunks -> forM_ thunks $ \(ThunkPackOpts dir upstream) -> packThunk dir upstream
  ObCommand_Repl component -> runRepl component
  ObCommand_Watch component -> watch component
  ObCommand_Internal icmd -> case icmd of
    ObInternal_RunStaticIO k -> liftIO  (unsafeLookupStaticPtr @(IO ()) k) >>= \case
      Nothing -> failWith $ "ObInternal_RunStaticIO: no such StaticKey: " <> T.pack (show k)
      Just p -> liftIO $ deRefStaticPtr p
    ObInternal_CLIDemo -> cliDemo

--TODO: Clean up all the magic strings throughout this codebase

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

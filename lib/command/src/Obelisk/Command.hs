{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Obelisk.Command where

import System.Process ()
import Control.Monad
import qualified Data.Binary as Binary
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import GHC.StaticPtr
import Options.Applicative
import System.Environment
import System.FilePath
import System.IO
import System.Posix.Process

import Obelisk.Command.Project
import Obelisk.Command.Deploy
import Obelisk.Command.Thunk
import Obelisk.Command.Repl
import Obelisk.Command.Run

data Args = Args
  { _args_noHandOffPassed :: Bool
  -- ^ This flag is actually handled outside of the optparse-applicative parser, but we detect whether
  -- it has gotten through in order to notify the user that it should only be passed once and as the very
  -- first argument
  , _args_command :: ObCommand
  }

args :: Parser Args
args = Args <$> noHandoff <*> obCommand

noHandoff :: Parser Bool
noHandoff = flag False True $ mconcat
  [ long "no-handoff"
  , help "Do not hand off execution to project-specific implementation of this command"
  , hidden
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

data ObInternal
   = ObInternal_RunStaticIO StaticKey

inNixShell' :: StaticPtr (IO ()) -> IO ()
inNixShell' p = withProjectRoot "." $ \root -> do
  progName <- getExecutablePath
  projectShell root False "ghc" $ unwords --TODO: shell escape
    [ progName
    , "internal"
    , "run-static-io"
    , encodeStaticKey $ staticKey p
    ]

obCommand :: Parser ObCommand
obCommand = hsubparser
    (mconcat
      [ command "init" $ info (ObCommand_Init <$> initSource) $ progDesc "Initialize an Obelisk project"
      , command "deploy" $ info (ObCommand_Deploy <$> deployCommand) $ progDesc "Prepare a deployment for an Obelisk project"
      , command "run" $ info (pure ObCommand_Run) $ progDesc "Run current project in development mode"
      , command "thunk" $ info (ObCommand_Thunk <$> thunkCommand) $ progDesc "Manipulate thunk directories"
      , command "repl" $ info (ObCommand_Repl <$> (strArgument (action "directory"))) $ progDesc "Open an interactive interpreter"
      , command "watch" $ info (ObCommand_Watch <$> (strArgument (action "directory")))$ progDesc "Watch directory for changes and update interactive interpreter"
      ])
  <|> subparser
    (mconcat
      [ internal
      , command "internal" (info (ObCommand_Internal <$> internalCommand) mempty)
      ])

deployCommand :: Parser DeployCommand
deployCommand = hsubparser $ mconcat
  [ command "init" $ info deployInitCommand $ progDesc "Initialize a deployment configuration directory"
  ]

deployInitCommand :: Parser DeployCommand
deployInitCommand = fmap DeployCommand_Init $ DeployInitOpts
  <$> strArgument (action "deploy-dir" <> metavar "DEPLOYDIR" <> help "Path to a directory that it will create")
  <*> strOption (long "ssh-key" <> metavar "SSHKEY" <> help "Path to an ssh key that it will symlink to")
  <*> some (strOption (long "hostname" <> metavar "HOSTNAME" <> help "hostname of the deployment target"))
  <*> strOption (long "remote" <> metavar "REMOTE" <> help "git remote to use for the src thunk")

data DeployCommand
  = DeployCommand_Init DeployInitOpts

data DeployInitOpts = DeployInitOpts
  { _deployInitOpts_ouputDir :: FilePath
  , _deployInitOpts_sshKey :: FilePath
  , _deployInitOpts_hostname :: [String]
  , _deployInitOpts_remote :: String
  }

internalCommand :: Parser ObInternal
internalCommand = subparser $ mconcat
  [ command "run-static-io" $ info (ObInternal_RunStaticIO <$> argument (eitherReader decodeStaticKey) (action "static-key")) mempty
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

data ThunkCommand
   = ThunkCommand_Update [FilePath]
   | ThunkCommand_Unpack [FilePath]
   | ThunkCommand_Pack [ThunkPackOpts]

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

main :: IO ()
main = do
  myArgs <- getArgs
  --TODO: We'd like to actually use the parser to determine whether to hand off,
  --but in the case where this implementation of 'ob' doesn't support all
  --arguments being passed along, this could fail.  For now, we don't bother
  --with optparse-applicative until we've done the handoff.
  let go as = do
        Args noHandoffPassed cmd <- handleParseResult (execParserPure parserPrefs argsInfo as)
        case noHandoffPassed of
          False -> return ()
           --TODO: Use a proper logging system with log levels and whatnot
          True -> hPutStrLn stderr "NOTICE: --no-handoff should only be passed once and as the first argument; ignoring"
        ob cmd
      handoffAndGo as = findProjectObeliskCommand "." >>= \case
        Nothing -> go as -- If not in a project, just run ourselves
        Just impl -> do
          -- Invoke the real implementation, using --no-handoff to prevent infinite recursion
          executeFile impl False ("--no-handoff" : myArgs) Nothing
  case myArgs of
    "--no-handoff" : as -> go as -- If we've been told not to hand off, don't hand off
    a:as -- Otherwise bash completion would always hand-off even if the user isn't trying to
      | "--bash-completion" `isPrefixOf` a
      && "--no-handoff" `elem` as -> go (a:as)
      | otherwise -> handoffAndGo (a:as)
    as -> handoffAndGo as

ob :: ObCommand -> IO ()
ob = \case
  ObCommand_Init source -> initProject source
  ObCommand_Deploy dc -> case dc of
    DeployCommand_Init deployOpts -> withProjectRoot "." $ \root -> do
      thunkPtr <- getThunkPtr root (_deployInitOpts_remote deployOpts)
      let deployDir = _deployInitOpts_ouputDir deployOpts
          sshKeyPath = _deployInitOpts_sshKey deployOpts
          hostname = _deployInitOpts_hostname deployOpts
      deployInit thunkPtr (root </> "config") deployDir sshKeyPath hostname
  ObCommand_Run -> inNixShell' $ static run
    -- inNixShell ($(mkClosure 'ghcidAction) ())
  ObCommand_Thunk tc -> case tc of
    ThunkCommand_Update thunks -> mapM_ updateThunkToLatest thunks
    ThunkCommand_Unpack thunks -> mapM_ unpackThunk thunks
    ThunkCommand_Pack thunks -> forM_ thunks $ \(ThunkPackOpts dir upstream) -> packThunk dir upstream
  ObCommand_Repl component -> runRepl component
  ObCommand_Watch component -> watch component
  ObCommand_Internal icmd -> case icmd of
    ObInternal_RunStaticIO k -> unsafeLookupStaticPtr @(IO ()) k >>= \case
      Nothing -> fail $ "ObInternal_RunStaticIO: no such StaticKey: " <> show k
      Just p -> deRefStaticPtr p
--TODO: Clean up all the magic strings throughout this codebase

encodeStaticKey :: StaticKey -> String
encodeStaticKey = T.unpack . decodeUtf8 . Base16.encode . LBS.toStrict . Binary.encode

decodeStaticKey :: String -> Either String StaticKey
decodeStaticKey s = case Base16.decode $ encodeUtf8 $ T.pack s of
  (b, "") -> case Binary.decodeOrFail $ LBS.fromStrict b of
    Right ("", _, a) -> pure a
    Right _ -> fail $ "decodeStaticKey: Binary.decodeOrFail didn't consume all input"
    Left (_, _, e) -> fail $ "decodeStaticKey: Binary.decodeOrFail failed: " <> show e
  _ -> fail $ "decodeStaticKey: could not decode hex string: " <> show s

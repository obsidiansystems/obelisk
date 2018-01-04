{-# LANGUAGE LambdaCase #-}
module Obelisk.Command where

import Options.Applicative
import System.Environment
import System.IO
import System.Posix.Process

import Obelisk.Command.Project
import Obelisk.Command.Thunk

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
  , help "Do not hand off execution to a project-specific implementation of this command"
  , hidden
  ]

argsInfo :: ParserInfo Args
argsInfo = info (args <**> helper) $ mconcat
  [ fullDesc
  , progDesc "Manage Obelisk projects"
  ]

data ObCommand
   = ObCommand_Init
   | ObCommand_Dev
   | ObCommand_Thunk ThunkCommand

obCommand :: Parser ObCommand
obCommand = hsubparser $ mconcat
  [ command "init" $ info (pure ObCommand_Init) $ progDesc "Initialize an Obelisk project"
  , command "dev" $ info (pure ObCommand_Dev) $ progDesc "Run the current project in development mode"
  , command "thunk" $ info (ObCommand_Thunk <$> thunkCommand) $ progDesc "Manipulate thunk directories"
  ]

data ThunkCommand
   = ThunkCommand_Update [FilePath]

thunkCommand :: Parser ThunkCommand
thunkCommand = hsubparser $ mconcat
  [ command "update" $ info (ThunkCommand_Update <$> some (strArgument (action "directory"))) $ progDesc "Update a thunk to the latest revision available"
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
  case myArgs of
    "--no-handoff" : as -> go as -- If we've been told not to hand off, don't hand off
    as -> do
      findProjectObeliskCommand "." >>= \case
        Nothing -> go as -- If we aren't in a project, just run ourselves
        Just impl -> do
          -- Invoke the real implementation, using --no-handoff to prevent infinite recursion
          executeFile impl False ("--no-handoff" : myArgs) Nothing

ob :: ObCommand -> IO ()
ob = \case
  ObCommand_Init -> initProject "."
  ObCommand_Dev -> putStrLn "Dev!"
  ObCommand_Thunk tc -> case tc of
    ThunkCommand_Update thunks -> mapM_ updateThunkToLatest thunks

--TODO: Clean up all the magic strings throughout this codebase

{-# LANGUAGE LambdaCase #-}
module Obelisk.Command where

import Obelisk.Command.Project
import Obelisk.Command.Thunk
import Options.Applicative
import System.Environment
import System.Posix.Process

data Args = Args
  { _args_handoff :: Bool
  , _args_command :: ObCommand
  }

args :: Parser Args
args = Args <$> noHandoff <*> obCommand

noHandoff :: Parser Bool
noHandoff = flag True False $ mconcat
  [ long "no-handoff"
  , help "Do not hand off execution to a project-specific implementation of this command"
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

main :: IO ()
main = do
  myArgs <- getArgs
  --TODO: We'd like to actually use the parser to determine whether to hand off,
  --but in the case where this implementation of 'ob' doesn't support all
  --arguments being passed along, this could fail.  For now, we don't bother
  --with optparse-applicative until we've done the handoff.

  --TODO: Doing handoff before command-line argument completion is currently too
  --slow; we need to make it faster, probably by avoiding calling nix-build when
  --we know things are fresh.
  let go = ob . _args_command =<< execParser argsInfo
  case myArgs of
    "--no-handoff" : _ -> go -- If we've been told not to hand off, don't hand off
    _ -> do
      findProjectObeliskCommand "." >>= \case
        Nothing -> go -- If we aren't in a project, just run ourselves
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

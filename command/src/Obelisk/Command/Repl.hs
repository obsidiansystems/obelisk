{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Obelisk.Command.Repl 
       ( runRepl
       , runInteractiveShell
       , watchInteractiveShell
       ) where

import Data.Monoid ((<>))
import System.Process

import Obelisk.Command.Project

-- | Run an interactive repl within a given path
runInteractiveShell :: FilePath -> IO ()
runInteractiveShell fp = runRepl fp "cabal new-repl "

-- | Watch and re-run an interactive repl within a given path every time a file is updated
watchInteractiveShell :: FilePath -> IO () 
watchInteractiveShell fp = runRepl fp $ "cabal new-repl " <> fp <> "\""

--TODO: modify the nix-shell --run to recognize when the common dir's files have changed as well.
-- | Run given shell command from a given directory
runRepl :: FilePath -> String -> IO ()
runRepl dir shellCommand = do
  findProjectRoot "." >>= \case
     Nothing -> putStrLn "'ob repl' must be used inside of an Obelisk project."
     Just pr -> do
       (_, _, _, ph) <- createProcess_ "runRepl" $ setCwd (Just pr) $ proc "nix-shell"
          [ "-A"
          , "shells.ghc"
          , "--run", "cd " <> dir <> "; " <> shellCommand 
          ]
       _ <- waitForProcess ph
       return ()

setCwd :: Maybe FilePath -> CreateProcess -> CreateProcess
setCwd fp cp = cp { cwd = fp }

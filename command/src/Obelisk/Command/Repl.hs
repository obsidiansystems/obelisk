{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Obelisk.Command.Repl (runRepl) where

import Data.Monoid ((<>))
import System.Process
import System.Directory
import GHC.IO.Handle
import GHC.IO.Handle.Types

import Obelisk.Command.Project

--TODO: modify the nix-shell --run to recognize when the common dir's files have changed as well.
runRepl :: FilePath -> Bool -> IO ()
runRepl dir runGhcid = do
  findProjectRoot "." >>= \case
     Nothing -> putStrLn "'ob repl' must be used inside of an Obelisk project."
     Just pr -> do
       (_, _, _, ph) <- createProcess_ "runRepl" $ setCwd (Just pr) $ proc "nix-shell"
          ["-A"
          , "shells.ghc"
          ,  "--run", whichCabal runGhcid
          ]
       waitForProcess ph
       return ()
  where
    whichCabal flag = case flag of
       True  -> "cd " <> dir <>"; ghcid -W -c\"cabal new-repl " <> dir <> "\""
       False -> "cd " <> dir <>"; cabal new-repl " <> dir <> "\""

setCwd :: Maybe FilePath -> CreateProcess -> CreateProcess
setCwd fp cp = cp{cwd = fp}

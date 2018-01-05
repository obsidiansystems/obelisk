module Obelisk.Command.Repl where

import Data.Monoid ((<>))
import System.Process 
import System.Directory

import Obelisk.Command.Project

-- TODO modify the nix-shell --run to recognize when the common dir's files have changed as well. 
runRepl :: FilePath -> IO ()
runRepl dir = do
  projectRoot <- findProjectObeliskCommand "."
  case projectRoot of 
       Nothing -> putStrLn "Project directory not found."
       Just pr -> do
         setCurrentDirectory pr
         callProcess "nix-shell" 
            [ "-A" 
            , "shells.ghc"
            , "--run"
            , "\'cd " <> dir <> "; ghcid -W -c\"cabal new-repl exe:" <> dir <> "\"\'"
            ]

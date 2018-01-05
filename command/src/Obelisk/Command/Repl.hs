module Obelisk.Command.Repl where

import Data.Monoid ((<>))
import System.Process 

-- TODO modify the nix-shell arguments to recognize when the common dir's files have changed as well. 
runRepl :: FilePath -> IO ()
runRepl dir = callProcess "nix-shell" 
  [ "-A" 
  , "shells.ghc"
  , "--run"
  , "\'cd " <> dir <> "; ghcid -W -c\"cabal new-repl exe:" <> dir <> "\"\'"
  ]

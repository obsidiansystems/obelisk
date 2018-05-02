{-# LANGUAGE OverloadedStrings #-}
module Obelisk.Command.Repl
  ( runRepl
  , runDev
  , watch
  , getFreePort
  ) where

import qualified Control.Exception as E
import Data.Maybe
import Data.Monoid ((<>))
import Network.Socket
import System.Process (callCommand)

import Obelisk.Command.Project

-- | Run an interactive repl within a given path
runRepl :: FilePath -> IO ()
runRepl dir = inProjectShell "ghc" $ "cd " <> dir <> "; cabal new-repl"

-- | Watch the given directory for errors and warnings
watch :: FilePath -> IO ()
watch dir = inProjectShell "ghc" $ "cd " <> dir <> "; ghcid -W --command='cabal new-repl'"

-- | Dev
runDev :: FilePath -> Maybe String -> IO ()
runDev dotGhci mcmd = callCommand $ unwords $ "ghcid" : ghcidOpts
  where
    ghcidOpts =
      [ "-W"
      , "--command='ghci -ghci-script " <> dotGhci <> "' "
      ] <> maybeToList (flip fmap mcmd $ \cmd -> " --test=$'" <> cmd <> "'")

getFreePort :: IO PortNumber
getFreePort = withSocketsDo $ do
  addr:_ <- getAddrInfo (Just defaultHints) (Just "127.0.0.1") (Just "0")
  E.bracket (open addr) close socketPort
  where
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      bind sock (addrAddress addr)
      return sock


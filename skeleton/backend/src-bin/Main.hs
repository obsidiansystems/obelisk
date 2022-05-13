module Main where

import Backend
import Frontend
import Obelisk.Backend

main :: IO ()
main = runBackend backend frontend

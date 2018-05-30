module Devel where

import Obelisk.Run

import Backend
import Frontend

main :: Int -> IO ()
main port = run port backend staticContent frontend


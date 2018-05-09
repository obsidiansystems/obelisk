module Backend where

import Common.Api
import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = Ob.backend Ob.def

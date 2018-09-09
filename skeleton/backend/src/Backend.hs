module Backend where

import Frontend
import qualified Obelisk.Backend as Ob

backend :: IO ()
backend = Ob.backend Ob.def
  { Ob._backendConfig_head = fst frontend
  }

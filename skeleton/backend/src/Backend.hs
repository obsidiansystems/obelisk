module Backend where

import Common.Api
import Frontend
import Obelisk.Backend as Ob

staticContent :: ReflexStaticContent
staticContent = StaticContent
  { _staticContent_head = staticHead
  , _staticContent_body = staticBody
  }

backend :: IO ()
backend = Ob.backend staticContent Ob.def

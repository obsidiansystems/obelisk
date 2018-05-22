module Backend where

import Common.Api
import Frontend
import Reflex.Dom
import Obelisk.Backend

staticContent :: ReflexStaticContent
staticContent = StaticContent
  { _staticContent_head = staticHead
  , _staticContent_body = staticBody
  }

backend :: IO ()
backend = runObeliskBackend staticContent def

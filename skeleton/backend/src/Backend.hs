{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-} 

module Backend where

import Control.Monad.IO.Class (liftIO)

import Common.Route
import Obelisk.Backend
import Obelisk.Route  

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      -- Pattern matches routes defined in Route.hs
      -- and passes route-based arguments to the Snap monad 
      BackendRoute_Missing :/ () -> pure () :: Snap () 
        
  , _backend_routeEncoder = fullRouteEncoder
  }

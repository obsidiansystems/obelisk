module Frontend where

import Obelisk.Frontend
import Obelisk.Generated.Static ()
import Obelisk.Route
import Obelisk.Route.Frontend
import Reflex.Dom

import Common.Route



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = frontendHead
  , _frontend_body = frontendBody
  }

frontendHead :: ObeliskWidget t route m => RoutedT t route m ()
frontendHead = do
  el "title" $ text "Obelisk Skeleton"

frontendBody :: ObeliskWidget t (R FrontendRoute) m => RoutedT t (R FrontendRoute) m ()
frontendBody = do
  el "h1" $ text "Welcome to Obelisk"
  el "p" $ text "Edit frontend/src/Frontend.hs to get started."

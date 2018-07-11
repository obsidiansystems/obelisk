{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Ref
import Reflex.Host.Class
import Data.Monoid
import Data.Text (Text)
import Obelisk.Route.Frontend
import Reflex.Dom.Core

type ObeliskWidget t route m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  , TriggerEvent t m
--  , HasDocument m --TODO: Would need StaticDomBuilderT to have this
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , EventWriter t (Endo route) m
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m. ObeliskWidget t route m => RoutedT t (route) m ())
  , _frontend_body :: !(forall t m x. (MonadWidget t m, HasJS x m, EventWriter t (Endo route) m) => RoutedT t (route) m ())
  , _frontend_routeEncoder :: !(Encoder (Either Text) (Either Text) (route) PageName)
  , _frontend_title :: !(route -> Text)
  , _frontend_notFoundRoute :: !(Text -> route)
  }


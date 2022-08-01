{-|
   Description:
     Wrapped DOM element that represents a link to an obelisk route.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Obelisk.Route.Frontend.RouteLink where

import Data.Dependent.Sum (DSum(..))
import qualified Data.Dependent.Map as DMap
import GHCJS.DOM.MouseEvent
import Reflex.Dom.Core

-- | Record of all modifier key states recognized by browsers
data ActiveModifierKeys = ActiveModifierKeys
  { _activeModifierKeys_control :: Bool
  , _activeModifierKeys_alt :: Bool
  , _activeModifierKeys_shift :: Bool
  , _activeModifierKeys_meta :: Bool
  }

-- | Return 'True' if any modifier key is active
anyActiveModifiers :: ActiveModifierKeys -> Bool
anyActiveModifiers ActiveModifierKeys
  { _activeModifierKeys_control = ctrl
  , _activeModifierKeys_alt = alt
  , _activeModifierKeys_shift = shift
  , _activeModifierKeys_meta = meta
  } = or [ctrl, alt, shift, meta]

-- | 'RouteLink' events require richer data on click than is usually exposed.
-- In particular, we need to check modifier keys that are active when a user
-- clicks on a link in order to determine whether we reflex should handle the
-- route change, or the browser's default behavior.
type family RouteLinkEventResultType (en :: EventTag) :: * where
  RouteLinkEventResultType 'ClickTag = ActiveModifierKeys
  RouteLinkEventResultType r = EventResultType r

-- | Newtype wrapper around the 'RouteLinkEventResultType' family
newtype RouteLinkEventResult en = RouteLinkEventResult
  { unRouteLinkEventResult :: RouteLinkEventResultType en
  }

-- | A wrapped DOM element which is aware of more event data that allows us
-- to distinguish between situations where the browser should handle link
-- behavior, and when we should handle link behavior.
newtype RouteLinkElement t = RouteLinkElement
  { unRouteLink :: Element RouteLinkEventResult GhcjsDomSpace t
  }

routeLinkElement
  :: forall t m a. (DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace)
  => (forall er. ElementConfig er t (DomBuilderSpace m))
  -> m a
  -> m (RouteLinkElement t, a)
routeLinkElement inputConfig inner = do
  (e, result) <- element "a" ghcjsRouteLinkConfig inner
  pure $ (RouteLinkElement e, result)
 where
  ghcjsRouteLinkConfig = inputConfig
    & elementConfig_eventSpec .~ (routeLinkEventSpec :: EventSpec GhcjsDomSpace RouteLinkEventResult)
  routeLinkEventSpec = def
    { _ghcjsEventSpec_filters = DMap.fromList
       [ Click :=> routeLinkClickFilter
       ]
    , _ghcjsEventSpec_handler = GhcjsEventHandler $ \(en, evt) ->
        fmap (fmap (regularToRouteLinkEventType en)) $ unGhcjsEventHandler
          (_ghcjsEventSpec_handler def)
          (en, evt)
    }
  routeLinkClickFilter :: GhcjsEventFilter RouteLinkEventResult 'ClickTag
  routeLinkClickFilter = GhcjsEventFilter $ \(GhcjsDomEvent rawEvt) -> do
    buttonIndex <- getButton rawEvt
    ctrl <- getCtrlKey rawEvt
    alt <- getAltKey rawEvt
    shift <- getShiftKey rawEvt
    meta <- getMetaKey rawEvt
    let modKeys = ActiveModifierKeys
          { _activeModifierKeys_control = ctrl
          , _activeModifierKeys_alt = alt
          , _activeModifierKeys_shift = shift
          , _activeModifierKeys_meta = meta
          }
        -- 0 is the primary button index, which is what we want to override
        -- with our own routing logic.
        eventFlags = case buttonIndex of
          0 | anyActiveModifiers modKeys -> preventDefault
          _ -> mempty
    pure $ (,) eventFlags $ pure $ Just $ RouteLinkEventResult modKeys

-- | Pass through the default event data for most events, since the only one
-- that is interesting for 'RouteLink' is the 'Click' event.
regularToRouteLinkEventType
  :: forall en.
     EventName en
  -> EventResult en
  -> RouteLinkEventResult en
-- Unfortunate! We have to spell out each case because the type family has to
-- be checked in each pattern.
regularToRouteLinkEventType en (EventResult r) = RouteLinkEventResult $ case en of
  Click -> error "regularToRouteLinkEventType: Click should never be encountered"
  Abort -> r
  Blur -> r
  Change -> r
  Contextmenu -> r
  Dblclick -> r
  Drag -> r
  Dragend -> r
  Dragenter -> r
  Dragleave -> r
  Dragover -> r
  Dragstart -> r
  Drop -> r
  Error -> r
  Focus -> r
  Input -> r
  Invalid -> r
  Keydown -> r
  Keypress -> r
  Keyup -> r
  Load -> r
  Mousedown -> r
  Mouseenter -> r
  Mouseleave -> r
  Mousemove -> r
  Mouseout -> r
  Mouseover -> r
  Mouseup -> r
  Mousewheel -> r
  Scroll -> r
  Select -> r
  Submit -> r
  Wheel -> r
  Beforecut -> r
  Cut -> r
  Beforecopy -> r
  Copy -> r
  Beforepaste -> r
  Paste -> r
  Reset -> r
  Search -> r
  Selectstart -> r
  Touchstart -> r
  Touchmove -> r
  Touchend -> r
  Touchcancel -> r

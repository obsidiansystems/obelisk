module Obelisk.Frontend
  ( ObeliskWidget
  , Frontend (..)
  , runFrontend
  , runFrontendWithConfigsAndCurrentRoute
  , renderFrontendHtml
  , removeHTMLConfigs
  , FrontendMode (..)
  , BootstrapMode (..)
  , parseBootstrapModeContent
  , FrontendWidgetT
  , module Obelisk.Frontend.Cookie
  ) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 810
import Data.Monoid ((<>))
#endif
#if __GLASGOW_HASKELL__ >= 906
import Control.Monad (when)
import Data.Functor (void)
#endif
#endif

import Control.Category
import Control.Lens
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Primitive
import Control.Monad.Reader
import Control.Monad.Ref
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.ByteString (ByteString)
import Data.Foldable (for_)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHCJS.DOM (currentDocument)
import GHCJS.DOM qualified as DOM
import GHCJS.DOM.History qualified as DOM
import GHCJS.DOM.Node (Node, removeChild_)
import GHCJS.DOM.NodeList (IsNodeList, getLength, item)
import GHCJS.DOM.ParentNode (querySelectorAll)
import GHCJS.DOM.Types qualified as DOM
import GHCJS.DOM.Window qualified as DOM
import Language.Javascript.JSaddle (JSM, JSString, MonadJSM, jsNull)
import Obelisk.Configs
import Obelisk.ExecutableConfig.Inject (injectExecutableConfigs)
import Obelisk.ExecutableConfig.Lookup qualified as Lookup
import Obelisk.Route.Frontend
import Reflex.Dom.Core
import Reflex.Host.Class
import Web.Cookie
import "ghcjs-dom" GHCJS.DOM.Document (getHead)
import "ghcjs-dom" GHCJS.DOM.Element (getAttribute)
import Prelude hiding ((.))

import Obelisk.Frontend.Cookie

type ObeliskWidget t route m =
  ( DomBuilder t m
  , MonadFix m
  , MonadHold t m
  , MonadSample t (Performable m)
  , MonadReflexCreateTrigger t m
  , PostBuild t m
  , PerformEvent t m
  , TriggerEvent t m
  , HasDocument m
  , MonadRef m
  , Ref m ~ Ref IO
  , MonadRef (Performable m)
  , Ref (Performable m) ~ Ref IO
  , MonadFix (Performable m)
  , PrimMonad m
  , Prerender t m
  , PrebuildAgnostic t route m
  , PrebuildAgnostic t route (Client m)
  , HasConfigs m
  , HasCookies m
  , MonadIO (Performable m)
  )

type PrebuildAgnostic t route m =
  ( SetRoute t route m
  , RouteToUrl route m
  , MonadFix m
  , HasConfigs m
  , HasConfigs (Performable m)
  )

data Frontend route = Frontend
  { _frontend_head :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  , _frontend_body :: !(forall t m. ObeliskWidget t route m => RoutedT t route m ())
  }

baseTag :: forall route t m. ObeliskWidget t route m => RoutedT t route m ()
baseTag =
  elAttr "base" ("href" =: "/") blank -- TODO: Figure out the base URL from the routes

-- | How 'runFrontend' should boot, selected by reserved head metadata:
--
-- > <meta name="obelisk-frontend-mode" content="client">
--
-- Absent metadata keeps the platform default ('platformFrontendMode').
data BootstrapMode
  = BootstrapMode_Platform
  | BootstrapMode_Client
  | BootstrapMode_ClientHash
  deriving (Eq, Ord, Show)

bootstrapModeMetaName :: Text
bootstrapModeMetaName = "obelisk-frontend-mode"

-- | Interpret the @content@ of one reserved bootstrap-mode metadata element.
-- An unrecognized or missing value is an error rather than an implicit
-- fallback to hydration.
parseBootstrapModeContent :: Maybe Text -> Either Text BootstrapMode
parseBootstrapModeContent = \case
  Nothing -> Left "content attribute missing"
  Just c
    | c == "client" -> Right BootstrapMode_Client
    | c == "client-hash" -> Right BootstrapMode_ClientHash
    | otherwise ->
        Left $
          "unrecognized content "
            <> T.pack (show c)
            <> "; expected \"client\" or \"client-hash\""

-- | Read the reserved bootstrap-mode metadata from the current document's
-- head. Must run before the immediate renderer replaces the head and body
-- (and before 'removeHTMLConfigs').
lookupBootstrapMode :: JSM BootstrapMode
lookupBootstrapMode = do
  mContents <- runMaybeT $ do
    doc <- MaybeT currentDocument
    hd <- MaybeT $ getHead doc
    els <- nodeListNodes =<< querySelectorAll hd ("meta[name=" <> bootstrapModeMetaName <> "]" :: Text)
    traverse (MaybeT . elementContent) els
  case mContents of
    Nothing -> pure BootstrapMode_Platform
    Just [] -> pure BootstrapMode_Platform
    Just [c] -> either bootstrapError pure $ parseBootstrapModeContent c
    Just _ -> bootstrapError "multiple metadata elements"
  where
    elementContent node =
      DOM.castTo DOM.Element node >>= \case
        Nothing -> pure Nothing
        Just e -> Just <$> getAttribute e ("content" :: JSString)
    bootstrapError msg =
      error $ T.unpack $ "runFrontend: invalid " <> bootstrapModeMetaName <> " metadata: " <> msg

-- | The 'FrontendMode' a bootstrap mode selects, and whether the current URL
-- may be reset to the root route. Client modes render immediately from the
-- current URL (path/query, or hash fragment) and never reset it.
bootstrapFrontendMode :: BootstrapMode -> FrontendMode
bootstrapFrontendMode = \case
  BootstrapMode_Platform -> platformFrontendMode
  BootstrapMode_Client ->
    FrontendMode
      { _frontendMode_hydrate = False
      , _frontendMode_adjustRoute = False
      }
  BootstrapMode_ClientHash ->
    FrontendMode
      { _frontendMode_hydrate = False
      , _frontendMode_adjustRoute = True
      }

removeHTMLConfigs :: JSM ()
removeHTMLConfigs = void $ runMaybeT $ do
  doc <- MaybeT currentDocument
  hd <- MaybeT $ getHead doc
  es <- nodeListNodes =<< querySelectorAll hd ("[data-obelisk-executable-config-inject-key]" :: Text)
  for_ es $ removeChild_ hd

-- | Collect all nodes in the node list.
--
-- TODO: this and the version in exe-config/ghcjs/lookup should be
-- upstreamed to jsaddle.
nodeListNodes :: (IsNodeList l, MonadJSM m) => l -> m [Node]
nodeListNodes es = do
  len <- getLength es
  -- Warning! len is unsigned. If the NodeList is empty, we must avoid
  -- accidentally traversing over [0..maxBound::Word]
  nodes <- traverse (item es) $ if len == 0 then [] else [0 .. len - 1]
  pure $ catMaybes nodes

setInitialRoute :: Bool -> JSM ()
setInitialRoute useHash = do
  window <- DOM.currentWindowUnchecked
  initialLocation <- DOM.getLocation window
  initialUri <- getLocationUri initialLocation
  history <- DOM.getHistory window
  DOM.replaceState history jsNull ("" :: Text)
    $ Just
    $ show
    $ setAdaptedUriPath useHash "/" initialUri

data FrontendMode = FrontendMode
  { _frontendMode_hydrate :: Bool
  -- ^ There is already a rendering of the DOM in place; hydrate it rather
  -- than building new DOM
  , _frontendMode_adjustRoute :: Bool
  -- ^ The page can't use regular routes, so encode routes into the hash
  -- instead
  }

-- | Run the frontend, setting the initial route to "/" on platforms where no
-- route exists ambiently in the context (e.g. anything but web).
-- Selects FrontendMode based on platform; this doesn't work for jsaddle-warp
runFrontend
  :: forall backendRoute route
   . Encoder Identity Identity (R (FullRoute backendRoute route)) PageName
  -> Frontend (R route)
  -> JSM ()
runFrontend validFullEncoder frontend = do
  -- The reserved head metadata and public configs must both be read before
  -- any rendering: the immediate renderer replaces the head and body, taking
  -- the metadata and injected configs with them.
  bootstrapMode <- lookupBootstrapMode
  let mode = bootstrapFrontendMode bootstrapMode
  configs <- lookupConfigs
  when (_frontendMode_hydrate mode) removeHTMLConfigs
  -- There's no fundamental reason that adjustRoute needs to control setting the
  -- initial route and *also* the useHash parameter; that's why these are
  -- separate here.  Currently they only differ for client-hash boot, which
  -- routes an explicit URL through its hash fragment and must not have that
  -- route reset to the root.
  when (bootstrapMode == BootstrapMode_Platform && _frontendMode_adjustRoute mode)
    $ setInitialRoute
    $ _frontendMode_adjustRoute mode
  runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend

-- A browser build arrives with server-rendered markup, a route already in the
-- page, and configs reachable from JSM, so it hydrates and leaves the route
-- alone. Every other platform renders from scratch and reads configs in IO.
#if defined(ghcjs_HOST_OS) || defined(wasm32_HOST_ARCH)
platformFrontendMode :: FrontendMode
platformFrontendMode = FrontendMode
  { _frontendMode_hydrate = True
  , _frontendMode_adjustRoute = False
  }

lookupConfigs :: JSM (Map Text ByteString)
lookupConfigs = Lookup.getConfigs
#else
platformFrontendMode :: FrontendMode
platformFrontendMode = FrontendMode
  { _frontendMode_hydrate = False
  , _frontendMode_adjustRoute = True
  }

lookupConfigs :: JSM (Map Text ByteString)
lookupConfigs = liftIO Lookup.getConfigs
#endif

runFrontendWithConfigsAndCurrentRoute
  :: forall backendRoute frontendRoute
   . FrontendMode
  -> Map Text ByteString
  -> Encoder Identity Identity (R (FullRoute backendRoute frontendRoute)) PageName
  -> Frontend (R frontendRoute)
  -> JSM ()
runFrontendWithConfigsAndCurrentRoute mode configs validFullEncoder frontend = do
  let ve = validFullEncoder . hoistParse errorLeft (reviewEncoder (rPrism $ _FullRoute_Frontend . _ObeliskRoute_App))
      errorLeft = \case
        Left _ -> error "runFrontend: Unexpected non-app ObeliskRoute reached the frontend. This shouldn't happen."
        Right x -> Identity x
      w
        :: ( RawDocument (DomBuilderSpace (HydrationDomBuilderT s DomTimeline m)) ~ DOM.Document
           , Ref (Performable m) ~ Ref IO
           , Ref m ~ Ref IO
           , DomBuilder DomTimeline (HydrationDomBuilderT s DomTimeline m)
           , MonadHold DomTimeline m
           , MonadRef m
           , MonadRef (Performable m)
           , MonadReflexCreateTrigger DomTimeline m
           , PerformEvent DomTimeline m
           , PostBuild DomTimeline m
           , PrimMonad m
           , MonadSample DomTimeline (Performable m)
           , DOM.MonadJSM m
           , MonadFix (Performable m)
           , MonadFix m
           , Prerender DomTimeline (HydrationDomBuilderT s DomTimeline m)
           , MonadIO (Performable m)
           )
        => (forall c. HydrationDomBuilderT s DomTimeline m c -> FloatingWidget () c)
        -> (forall c. HydrationDomBuilderT s DomTimeline m c -> FloatingWidget () c)
        -> FloatingWidget () ()
      w appendHead appendBody = do
        rec switchover <- runRouteViewT ve switchover (_frontendMode_adjustRoute mode) $ do
              (switchover'', fire) <- newTriggerEvent
              mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendHead . runConfigsT configs))) $ do
                -- The order here is important - baseTag has to be before headWidget!
                baseTag
                _frontend_head frontend
              mapRoutedT (mapSetRouteT (mapRouteToUrlT (appendBody . runConfigsT configs))) $ do
                _frontend_body frontend
                switchover' <- case _frontendMode_hydrate mode of
                  True -> lift $ lift $ lift $ lift $ HydrationDomBuilderT $ asks _hydrationDomBuilderEnv_switchover
                  False -> getPostBuild
                performEvent_ $ liftIO (fire ()) <$ switchover'
              pure switchover''
        pure ()
  if _frontendMode_hydrate mode
    then runHydrationWidgetWithHeadAndBody (pure ()) w
    else runImmediateWidgetWithHeadAndBody w

type FrontendWidgetT r = RoutedT DomTimeline r (SetRouteT DomTimeline r (RouteToUrlT r (ConfigsT (CookiesT (HydratableT (PostBuildT DomTimeline (StaticDomBuilderT DomTimeline (PerformEventT DomTimeline DomHost))))))))

renderFrontendHtml
  :: MonadIO m
  => Map Text ByteString
  -> Cookies
  -> (r -> Text)
  -> r
  -> Frontend r
  -> FrontendWidgetT r ()
  -> FrontendWidgetT r ()
  -> m ByteString
renderFrontendHtml configs cookies urlEnc route frontend headExtra bodyExtra = do
  -- TODO: We should probably have a "NullEventWriterT" or a frozen reflex timeline
  html <- fmap snd $
    liftIO $
      renderStatic $
        runHydratableT $
          fmap fst $
            runCookiesT cookies $
              runConfigsT configs $
                flip runRouteToUrlT urlEnc $
                  runSetRouteT $
                    flip runRoutedT (pure route) $
                      -- The lang attribute only needs to exist server-side: hydration appends
                      -- into the existing head/body and never replaces the html element.
                      elAttr "html" ("lang" =: "en") $ do
                        el "head" $ do
                          baseTag
                          injectExecutableConfigs configs
                          _frontend_head frontend
                          headExtra
                        el "body" $ do
                          _frontend_body frontend
                          bodyExtra
  pure $ "<!DOCTYPE html>" <> html

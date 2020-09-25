{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Common.Route where

-- You will probably want these imports for composing Encoders.
--import Prelude hiding (id, (.))
--import Control.Category

import Control.Monad.Except (MonadError)
import Data.Functor.Identity
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Universe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.URI as URI

import Obelisk.Route
import Obelisk.Route.TH

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  --BackendRoute_Static :: BackendRoute () -- Should error on checking
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRouteA a where
  FrontendRouteA_Main :: FrontendRouteA ()
  FrontendRouteA_Int :: FrontendRouteA Int

data FrontendRouteB a where
  FrontendRouteB_Main :: FrontendRouteB ()
  FrontendRouteB_Text :: FrontendRouteB Text

-- TODO remove or provide TH
instance Universe (SomeDomain DomainRoute) where
  universe = [SomeDomain DomainRoute_A, SomeDomain DomainRoute_B]

data DomainRoute :: * -> * where
  DomainRoute_A :: DomainRoute (R (FullRoute BackendRoute FrontendRouteA))
  DomainRoute_B :: DomainRoute (R (FullRoute BackendRoute FrontendRouteB))

--fullRouteEncoder :: RouteConfig -> Encoder (Either Text) Identity (R DomainRoute) DomainPageName
--fullRouteEncoder (RouteConfig config) = domainEncoder $ \case
--  DomainRoute_A -> DomainResult (domain 0) domainAFullRouteEncoder
--  DomainRoute_B -> DomainResult (domain 1) domainBFullRouteEncoder
--  where routes = T.lines $ T.decodeUtf8 config
--        domain n = ConcreteDomain $ fromMaybe (error $ "route " <> show n <> " missing") $ URI.parseURI $ T.unpack $ routes !! n

myMkFullRouteEncoder :: DomainRoute (R (FullRoute b f)) -> Encoder (Either Text) Identity (R (FullRoute b f)) PageName
myMkFullRouteEncoder = \case
  DomainRoute_A -> domainAFullRouteEncoder
  DomainRoute_B -> domainBFullRouteEncoder

-- ob run needs something like: DomainRoute -> http/https
-- backend routing needs: hostName/port -> DomainRoute
-- ob run also needs: [port] OR DomainRoute -> port with universe of DomainRoute
baseRoute :: RouteConfig -> DomainRoute a -> URI.URI
baseRoute (RouteConfig rawRouteConfig) = \case
  DomainRoute_A -> fromMaybe (error "route A is bad") $ URI.parseURI $ T.unpack $ routeConfig !! 0
  DomainRoute_B -> fromMaybe (error "route B is bad") $ URI.parseURI $ T.unpack $ routeConfig !! 1
  where
    routeConfig = T.lines $ T.decodeUtf8 rawRouteConfig

backendSegment :: (Applicative check, MonadError Text parse) => BackendRoute a -> SegmentResult check parse a
backendSegment = \case
  BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty

domainAFullRouteEncoder :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRouteA)) PageName
domainAFullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  backendSegment
  (\case
    FrontendRouteA_Main -> PathEnd $ unitEncoder mempty
    FrontendRouteA_Int -> PathSegment "int" readShowEncoder)

domainBFullRouteEncoder :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRouteB)) PageName
domainBFullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Missing :/ ())
  backendSegment
  (\case
    FrontendRouteB_Main -> PathEnd $ unitEncoder mempty
    FrontendRouteB_Text -> PathSegment "text" singlePathSegmentEncoder)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRouteA
  , ''FrontendRouteB
  , ''DomainRoute
  ]

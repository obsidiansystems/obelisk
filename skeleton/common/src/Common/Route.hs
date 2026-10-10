module Common.Route where

import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.Text (Text)
import Obelisk.Route
import Obelisk.Route.TH



data BackendRoute :: Type -> Type where
  BackendRoute_Api :: BackendRoute ()

data FrontendRoute :: Type -> Type where
  FrontendRoute_Main :: FrontendRoute ()



concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]



checkedFullRouteEncoder :: Encoder Identity Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
checkedFullRouteEncoder = case checkEncoder fullRouteEncoder of
  Left e -> error $ show e
  Right x -> x

fullRouteEncoder
  :: Encoder (Either Text) Identity (R (FullRoute BackendRoute FrontendRoute)) PageName
fullRouteEncoder = mkFullRouteEncoder
  (FullRoute_Backend BackendRoute_Api :/ ())
  (\case BackendRoute_Api -> PathSegment "api" $ unitEncoder mempty)
  (\case FrontendRoute_Main -> PathEnd $ unitEncoder mempty)

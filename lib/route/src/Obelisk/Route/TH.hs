module Obelisk.Route.TH (deriveRouteComponent) where

import Data.Constraint.Extras.TH
import Data.GADT.Show.TH
import Data.GADT.Compare.TH
import Data.Universe.Some.TH
import Language.Haskell.TH

-- | Derive all the typeclasses needed for a RouteComponent type.  The argument should be the name of a type of kind @k -> *@
deriveRouteComponent :: Name -> Q [Dec]
deriveRouteComponent x = concat <$> traverse ($ x)
  [ deriveGShow
  , deriveGEq
  , deriveGCompare
  , deriveUniverseSome
  , deriveArgDict
  ]

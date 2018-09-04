{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Common.Route where

import Prelude hiding ((.))

import Control.Category
import Control.Monad.Except
import Data.Text
import Data.Functor.Sum
import Obelisk.Route

backendRouteEncoder
  :: forall check parse. (check ~ parse, MonadError Text parse)
  => Encoder check parse (R (Sum Void1 (ObeliskRoute IndexOnlyRoute))) PageName
backendRouteEncoder = Encoder $ do
  let componentEncoder = shadowEncoder void1Encoder $ obeliskRouteComponentEncoder indexOnlyRouteComponentEncoder
  restEncoder <- checkObeliskRouteRestEncoder indexOnlyRouteRestEncoder
  checkEncoder $ pathComponentEncoder (componentEncoder . someSumEncoder) $ \case
    InL backendRoute -> case backendRoute of {}
    InR obeliskRoute -> runValidEncoderFunc restEncoder obeliskRoute

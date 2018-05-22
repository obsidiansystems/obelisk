{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Rank2Types #-}

module Obelisk.Frontend where

import Data.ByteString (ByteString)
import Reflex.Dom

-- | Specify a reflex frontend.
data ObeliskFrontend = ObeliskFrontend
  { _obeliskFrontend_body :: forall x. Widget x ()
  -- ^ The content of <body> after the JavaScript is loaded
  }


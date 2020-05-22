{-# LANGUAGE TypeApplications #-}
module Main where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Control.Lens (view, from)

import Data.Text (Text)
import qualified Data.Text as Text

import Obelisk.Route

main :: IO ()
main = defaultMain $ testGroup "Route Tests"
  [ testGroup "Provided CanSafeShowRead Instances are law abiding"
    [ testProperty "Int" $ p (arbitrary @Int)
    , testProperty "String" $ p (arbitrary @String)
    , testProperty "Text" $ p (Text.pack . getUnicodeString <$> arbitrary @UnicodeString)
    ]
  ]
  where
    p g = forAll g isCanSafeShowReadValidFor

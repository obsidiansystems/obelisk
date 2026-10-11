module Main (main) where

import Data.Text (Text)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Obelisk.Frontend (BootstrapMode (..), parseBootstrapModeContent)

-- The reserved obelisk-frontend-mode metadata selects how runFrontend boots.
-- Absent metadata is the platform default; a present value must be exactly
-- "client" or "client-hash", and anything else - including a missing content
-- attribute - is an error rather than an implicit hydration fallback.
main :: IO ()
main =
  defaultMain $
    testGroup
      "parseBootstrapModeContent"
      [ testCase "client" $
          parseBootstrapModeContent (Just "client") @?= Right BootstrapMode_Client
      , testCase "client-hash" $
          parseBootstrapModeContent (Just "client-hash") @?= Right BootstrapMode_ClientHash
      , testCase "unknown value is rejected" $
          case parseBootstrapModeContent (Just "hydrate") of
            Left _ -> pure ()
            Right m -> assertFail ("unexpected success: " <> show m)
      , testCase "empty content is rejected" $
          case parseBootstrapModeContent (Just "") of
            Left _ -> pure ()
            Right m -> assertFail ("unexpected success: " <> show m)
      , testCase "missing content attribute is rejected" $
          case parseBootstrapModeContent Nothing of
            Left _ -> pure ()
            Right m -> assertFail ("unexpected success: " <> show m)
      , testCase "rejection names the offending value" $
          case parseBootstrapModeContent (Just "native" :: Maybe Text) of
            Left e -> e @?= "unrecognized content \"native\"; expected \"client\" or \"client-hash\""
            Right _ -> assertFail "unexpected success"
      ]
  where
    assertFail :: String -> IO ()
    assertFail m = ioError (userError m)

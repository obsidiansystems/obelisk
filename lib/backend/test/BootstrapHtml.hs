-- Behavior of the portable client-bootstrap HTML: it must select client boot,
-- inject exactly the public configs (base64, decodable), point at the fixed
-- web-bundle entry URLs, and never carry backend config.
module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , setCurrentDirectory
  )
import System.FilePath ((</>))
import System.IO.Temp (withTempDirectory)
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

import Obelisk.Backend
  ( GhcjsAppUrls (..)
  , getPublicConfigs
  , renderClientBootstrapHtml
  )

jsUrls, wasmUrls :: GhcjsAppUrls
jsUrls =
  GhcjsAppUrls
    { _ghcjsAppUrls_allJs = "/ghcjs/all.js"
    , _ghcjsAppUrls_wasm = Nothing
    }
wasmUrls =
  GhcjsAppUrls
    { _ghcjsAppUrls_allJs = "/ghcjs/all.js"
    , _ghcjsAppUrls_wasm = Just "/ghcjs/frontend.wasm"
    }

publicValue, secretValue :: ByteString
publicValue = "backend-origin.example\n"
secretValue = "hunter2-backend-secret"

publicConfigs :: Map Text ByteString
publicConfigs =
  Map.fromList
    [ ("common/route", publicValue)
    , ("frontend/title", "Example")
    ]

main :: IO ()
main =
  defaultMain $
    testGroup
      "renderClientBootstrapHtml"
      [ testCase "selects client boot through the reserved metadata" $ do
          html <- renderHtml jsUrls
          assertInfix "obelisk-frontend-mode" html
          assertInfix "content=\"client\"" html
      , testCase "injects public config base64-decodably" $ do
          html <- renderHtml jsUrls
          assertInfix "data-obelisk-executable-config-inject-key=\"common/route\"" html
          assertInfix (decodeUtf8With lenientDecode (B64.encode publicValue)) html
          assertInfix "data-obelisk-executable-config-inject-key=\"frontend/title\"" html
      , testCase "points at the fixed web-bundle entry URLs" $ do
          jsHtml <- renderHtml jsUrls
          assertInfix "src=\"/ghcjs/all.js\"" jsHtml
          assertBool "js target must not reference frontend.wasm" $
            not ("/ghcjs/frontend.wasm" `T.isInfixOf` jsHtml)
          wasmHtml <- renderHtml wasmUrls
          assertInfix "src=\"/ghcjs/all.js\"" wasmHtml
          assertInfix "/ghcjs/frontend.wasm" wasmHtml
      , testCase "getPublicConfigs reads common and frontend but never backend" $
          withTempDirectory "/tmp" "obelisk-bootstrap-test" $ \dir -> do
            createDirectoryIfMissing True (dir </> "config/common")
            createDirectoryIfMissing True (dir </> "config/frontend")
            createDirectoryIfMissing True (dir </> "config/backend")
            BS.writeFile (dir </> "config/common/route") publicValue
            BS.writeFile (dir </> "config/frontend/title") "Example"
            BS.writeFile (dir </> "config/backend/secret") secretValue
            old <- getCurrentDirectory
            setCurrentDirectory dir
            configs <- getPublicConfigs
            htmlBs <- renderClientBootstrapHtml configs jsUrls
            let html = decodeUtf8With lenientDecode htmlBs
            setCurrentDirectory old
            Map.lookup "common/route" configs @?= Just publicValue
            Map.lookup "backend/secret" configs @?= Nothing
            assertBool "raw backend config value must not appear" $
              not (decodeUtf8With lenientDecode secretValue `T.isInfixOf` html)
            assertBool "base64 backend config value must not appear" $
              not (decodeUtf8With lenientDecode (B64.encode secretValue) `T.isInfixOf` html)
      ]
  where
    renderHtml urls = decodeUtf8With lenientDecode <$> renderClientBootstrapHtml publicConfigs urls
    assertInfix needle hay =
      assertBool
        ("expected to find " <> show needle <> " in rendered HTML")
        (needle `T.isInfixOf` hay)

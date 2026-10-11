-- | Emit the portable client-bootstrap HTML for an Obelisk web bundle.
--
-- > obelisk-frontend-html js|wasm
--
-- Reads the public configs (config/common, config/frontend) from the working
-- directory - absent config is valid - and writes the bootstrap HTML to
-- stdout. The HTML expects the compiled frontend at /ghcjs/ and the static
-- assets at /static/ of the same origin, as the web bundle lays them out.
module Main (main) where

import Data.ByteString qualified as BS
import Obelisk.Backend (GhcjsAppUrls (..), getPublicConfigs, renderClientBootstrapHtml)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

webBundleUrls :: String -> Maybe GhcjsAppUrls
webBundleUrls target = case target of
  "js" -> Just GhcjsAppUrls
    { _ghcjsAppUrls_allJs = "/ghcjs/all.js"
    , _ghcjsAppUrls_wasm = Nothing
    }
  "wasm" -> Just GhcjsAppUrls
    { _ghcjsAppUrls_allJs = "/ghcjs/all.js"
    , _ghcjsAppUrls_wasm = Just "/ghcjs/frontend.wasm"
    }
  _ -> Nothing

main :: IO ()
main = getArgs >>= \case
  [target] | Just urls <- webBundleUrls target -> do
    configs <- getPublicConfigs
    html <- renderClientBootstrapHtml configs urls
    BS.putStr html
  _ -> do
    hPutStrLn stderr "usage: obelisk-frontend-html js|wasm"
    exitFailure

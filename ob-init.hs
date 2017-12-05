#! /usr/bin/env nix-shell
#! nix-shell "./script-env/ob-init.nix" -i "runghc"
--TODO: Don't depend on the user's environment - everything should be based on nixpkgs via reflex-platform
{-|
 Description: This script prompts user for the project name, and
 initializes the Obelisk submodules along with appropriate
 directory layout fit for best developer environment results.
 This script is to be used within an existing
 github repository and assumes that you have already
 configured/added your SSH key with both github and gitlab.
-}


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import System.Process (callProcess)
import System.Directory ( doesDirectoryExist
                        , createDirectoryIfMissing
                        , doesFileExist)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Monoid ((<>))

main :: IO ()
main = do
  --TODO consider passing an arg instead of using a prompt
  putStr "Project name: "
  projectName <- Text.getLine
  doesDirectoryExist "obelisk" >>= \case
    True -> putStrLn "Skipping obelisk (already exists)"
    False -> do
      callProcess "git"
        [ "submodule"
        , "add"
        , "-b"
        , "develop"
        , "git@github.com:obsidiansystems/obelisk.git"
        ]
      callProcess "git"
        [ "submodule"
        , "update"
        , "--init"
        , "--recursive"
        ]
  mkdirs ["common/src", "frontend/src", "backend/src", "static", "config"]
  let defNix    = "default.nix" :: FilePath
  let frontend  = "frontend/src/Main.hs" :: FilePath
  let backend   = "backend/src/Main.hs" :: FilePath
  createFileIfMissing defNix $ nixExpr projectName
  createFileIfMissing frontend frontSrc
  createFileIfMissing backend backSrc


--TODO: Consider using fileEmbed to generate default.nix in initializing folder
nixExpr :: Text -- ^ The name of the project; this must be a valid Cabal package name
        -> Text
nixExpr projectName = Text.unlines
  [ "{}: (import ./obelisk {}).mkDerivation {"
  , "  name = \"" <> projectName <> "\";"
  , "  version = \"0.1\";"
  , "  commonDepends = p: with p; ["
  , "     data-default"
  , "     file-embed"
  , "  ];"
  , "  frontendDepends = p: with p; ["
  , "     data-default"
  , "     file-embed"
  , "     obelisk-asset-th"
  , "     ghcjs-dom"
  , "     reflex"
  , "     reflex-dom"
  , "     these"
  , "  ];"
  , "  backendDepends = p: with p; ["
  , "     data-default"
  , "     resource-pool"
  , "     snap"
  , "     snap-core"
  , "     snap-loader-static"
  , "     snap-server"
  , "  ];"
  , "}"
  ]


frontSrc :: Text
frontSrc = Text.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "import Reflex.Dom"
  , "import GHCJS.DOM.Types (JSM, MonadJSM)"
	, ""
	, "#ifdef MIN_VERSION_jsaddle_warp"
	, "import Language.Javascript.JSaddle.Warp (run)"
  , "#endif"
	, ""
	, "#ifdef MIN_VERSION_jsaddle_wkwebview"
	, "import Language.Javascript.JSaddle.WKWebView (runFile)"
	, "#endif"
	, ""
	, "#if defined(__GHCJS__) && !defined(MIN_VERSION_jsaddle_warp) && !defined(MIN_VERSION_jsaddle_wkwebview)"
	, "runApp :: JSM () -> IO ()"
	, "runApp = id"
	, "#endif"
	, ""
	, "#ifdef MIN_VERSION_jsaddle_warp"
	, "runApp :: JSM () -> IO ()"
	, "runApp = run 3913"
	, "#endif"
	, ""
	, "#ifdef MIN_VERSION_jsaddle_wkwebview"
	, "runApp :: JSM () -> IO ()"
	, "runApp = runFile \"index.html\" \"\""
	, "#endif"
	, ""
  , "main = mainWidget $ text \"Hello new project\""
  ]

backSrc :: Text
backSrc = Text.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , ""
  , "import Data.Default"
  , "import Obelisk.Backend"
  , "import Obelisk.Backend.Snap"
  , "import Snap"
  , ""
  , "main :: IO ()"
  , "main = quickHttpServe $ rootHandler"
  , ""
  , "rootHandler :: Snap ()"
  , "rootHandler = route [(\"\", serveApp \"\" $ def)]"
  ]


mkdirs :: [FilePath] -> IO ()
mkdirs = mapM_ $ createDirectoryIfMissing True

createFileIfMissing :: FilePath -> Text -> IO ()
createFileIfMissing aFile content =
  doesFileExist aFile >>= \case
    True -> putStrLn ("Skipping " ++ show aFile ++ "(already exist)")
    False -> Text.writeFile aFile content

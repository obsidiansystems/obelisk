{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Asset.Cabal where

import Data.List qualified as L
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory
import System.FilePath

-- | A generated single-module package
data SimplePkg = SimplePkg
  { _simplePkg_name :: Text
  , _simplePkg_moduleName :: Text
  , _simplePkg_moduleContents :: Text
  , _simplePkg_dependencies :: [Text]
  }

cabalFile :: SimplePkg -> Text
cabalFile (SimplePkg packageName moduleName _ deps) =
  T.unlines
    [ "name: " <> packageName
    , "version: 0"
    , "cabal-version: >= 1.2"
    , "build-type: Simple"
    , ""
    , "library"
    , "  hs-source-dirs: src"
    , "  build-depends:"
    , "    " <> T.intercalate ", " deps
    , "  exposed-modules: " <> moduleName
    ]

writeCabalProject :: FilePath -> SimplePkg -> IO ()
writeCabalProject target pkg = do
  createDirectoryIfMissing True target
  T.writeFile (target </> T.unpack (_simplePkg_name pkg) <.> "cabal") $ cabalFile pkg
  let moduleName = _simplePkg_moduleName pkg
      (modName', moduleDirPath) = case L.uncons (reverse $ T.splitOn "." moduleName) of
        Nothing -> error $ "writeStaticProject: invalid module name " <> T.unpack moduleName
        Just (name, parents) -> (name, target </> "src" </> T.unpack (T.intercalate "/" $ reverse parents))
  createDirectoryIfMissing True moduleDirPath
  T.writeFile (moduleDirPath </> T.unpack modName' <.> "hs") $ _simplePkg_moduleContents pkg

import qualified Data.Text as T
import Obelisk.Asset.Cabal
import Obelisk.Asset.Gather
import Obelisk.Asset.Symlink
import System.Environment
import System.FilePath

main :: IO ()
main = do
  --TODO: Usage
  [root, haskellTarget, packageName, moduleName, fileTarget] <- getArgs
  paths <- gatherHashedPaths root
  writeCabalProject haskellTarget $ SimplePkg
    { _simplePkg_name = T.pack packageName
    , _simplePkg_moduleName = T.pack moduleName
    , _simplePkg_dependencies = map T.pack
      [ "base"
      , "filepath"
      , "obelisk-asset-manifest"
      , "template-haskell"
      ]
    , _simplePkg_moduleContents = T.pack $ unlines
      [ "{-# Language BangPatterns #-}"
      , "{-# Language CPP #-}"
      , "{-# Language OverloadedStrings #-}"
      , "{-|"
      , "  Description:"
      , "    Automatically generated module that provides the 'static' TH function"
      , "    to generate paths to static assets."
      , "-}"
      , "module " <> moduleName <> " ( static ) where"
      , ""
      , "import Obelisk.Asset.Gather"
      , "import Language.Haskell.TH"
      , "import Language.Haskell.TH.Syntax"
      , "import System.FilePath"
      , ""
      , "-- | Produces a string literal with the hashed path of the file"
      , "assetPath :: FilePath -> FilePath -> Q Exp"
      , "assetPath root relativePath = do"
      , "  qAddDependentFile $ root </> relativePath"
      , "  LitE . StringL . (prefix</>) <$> runIO (toHashedPath root relativePath)"
      , ""
      , "-- | Produces a string literal with the raw (i.e., unhashed) path of the file"
      , "assetPathRaw :: FilePath -> Q Exp"
      , "assetPathRaw fp = returnQ $ LitE $ StringL $ prefix </> fp"
      , ""
      , "prefix :: FilePath"
      , "prefix = \"/static\""
      , ""
      , "static :: FilePath -> Q Exp"
      , "#ifdef PASSTHRU"
      , "static = assetPathRaw"
      , "#else"
      , "static = assetPath " <> show root
      , "#endif"
      ]
    }
  copyAndSymlink paths root fileTarget

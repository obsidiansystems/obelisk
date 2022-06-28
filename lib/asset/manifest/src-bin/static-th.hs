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
      , "obelisk-asset-manifest"
      , "template-haskell"
      ]
    , _simplePkg_moduleContents = T.pack $ unlines
      [ "{-# Language CPP #-}"
      , "{-|"
      , "  Description:"
      , "    Automatically generated module that provides the 'static' TH function"
      , "    to generate paths to static assets."
      , "-}"
      , "module " <> moduleName <> " ( static, staticFilePath ) where"
      , ""
      , "import Obelisk.Asset.TH"
      , "import Language.Haskell.TH"
      , ""
      , "static, staticFilePath :: FilePath -> Q Exp"
      , "#ifdef OBELISK_ASSET_PASSTHRU"
      , "static = staticAssetRaw"
      , "staticFilePath =  staticAssetFilePathRaw \"static.out\""
      , "#else"
      , "static = staticAssetHashed " <> show root
      , "staticFilePath = staticAssetFilePathRaw " <> show root
      , "#endif"
      ]
    }
  copyAndSymlink paths root fileTarget

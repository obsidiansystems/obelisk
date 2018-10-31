import qualified Data.Text as T
import Obelisk.Asset.Gather
import Obelisk.Asset.Promoted
import Obelisk.Asset.Symlink
import System.Environment

main :: IO ()
main = do
  --TODO: Usage
  [root, rootLocalPath, haskellTarget, packageName, moduleName, fileTarget] <- getArgs
  paths <- gatherHashedPaths root
  writeStaticProject paths haskellTarget $ StaticConfig
    { _staticConfig_packageName = T.pack packageName
    , _staticConfig_moduleName = T.pack moduleName
    , _staticConfig_rootPath = rootLocalPath
    }
  copyAndSymlink paths root fileTarget

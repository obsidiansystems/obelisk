import Data.Text qualified as T
import System.Environment

import Obelisk.Asset.Gather
import Obelisk.Asset.Promoted
import Obelisk.Asset.Symlink

main :: IO ()
main = do
  args <- getArgs
  let (moduleOnly, root, haskellTarget, packageName, moduleName, fileTarget) = case args of
        ["--module-only", r, h, m, f] -> (True, r, h, "", m, f)
        [r, h, p, m, f] -> (False, r, h, p, m, f)
        _ -> error "Usage: obelisk-asset-manifest-generate [--module-only] <root> <haskellTarget> [<packageName>] <moduleName> <fileTarget>"
  paths <- gatherHashedPaths root
  if moduleOnly
    then writeStaticModule paths haskellTarget (T.pack moduleName)
    else
      writeStaticProject paths haskellTarget $
        StaticConfig
          { _staticConfig_packageName = T.pack packageName
          , _staticConfig_moduleName = T.pack moduleName
          }
  copyAndSymlink paths root fileTarget

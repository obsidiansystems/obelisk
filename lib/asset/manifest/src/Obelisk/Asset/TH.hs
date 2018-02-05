module Obelisk.Asset.TH
  ( assetPath
  ) where

import Obelisk.Asset.Gather

import Language.Haskell.TH.Syntax
import System.FilePath.Posix

-- | Produces a string literal with the hashed path of the file
assetPath :: FilePath -> FilePath -> Q Exp
assetPath root relativePath = do
  qAddDependentFile $ root </> relativePath
  LitE . StringL <$> runIO (toHashedPath root relativePath)

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Obelisk.Asset.Promoted
  ( writeStaticProject
  , declareStatic
  , StaticConfig (..)
  ) where

import Obelisk.Asset.Gather

import Data.Foldable
import qualified Data.List as L
import Language.Haskell.TH (runQ, pprint)
import Language.Haskell.TH.Syntax hiding (lift)
import GHC.TypeLits
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Writer
import System.FilePath
import System.Directory
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as Map

data StaticConfig = StaticConfig
  { _staticConfig_packageName :: Text --TODO: Better type
  , _staticConfig_moduleName :: Text --TODO: Better type
  , _staticConfig_rootPath :: FilePath
  }

writeStaticProject :: Map FilePath FilePath -> FilePath -> StaticConfig -> IO ()
writeStaticProject paths target cfg = do
  createDirectoryIfMissing True target
  T.writeFile (target </> T.unpack (_staticConfig_packageName cfg) <.> "cabal") $ staticCabalFile cfg
  let modName = _staticConfig_moduleName cfg
      (modName', moduleDirPath) = case L.uncons (reverse $ T.splitOn "." modName) of
        Nothing -> error $ "writeStaticProject: invalid module name " <> T.unpack modName
        Just (name, parents) -> (name, target </> "src" </> T.unpack (T.intercalate "/" $ reverse parents))
  createDirectoryIfMissing True moduleDirPath
  modContents <- staticModuleFile modName paths
  T.writeFile (moduleDirPath </> T.unpack modName' <.> "hs") $ appendRootPath (_staticConfig_rootPath cfg) modContents

appendRootPath :: FilePath -> Text -> Text
appendRootPath fp txt = T.unlines
  [ txt
  , "staticRootPath :: FilePath"
  , "staticRootPath = \"" <> T.pack fp <> "\""
  ]

staticCabalFile :: StaticConfig -> Text
staticCabalFile cfg = T.unlines
  [ "name: " <> _staticConfig_packageName cfg
  , "version: 0"
  , "cabal-version: >= 1.2"
  , "build-type: Simple"
  , ""
  , "library"
  , "  hs-source-dirs: src"
  , "  build-depends:"
  , "    base,"
  , "    ghc-prim,"
  , "    text"
  , "  exposed-modules: " <> _staticConfig_moduleName cfg
  ]

staticModuleFile :: Text -> Map FilePath FilePath -> IO Text
staticModuleFile moduleName paths = do
  decs <- runQ $ fmap toList $ execWriterT $ staticClassWithInstances paths
  return $ T.unlines
    [ "{-# LANGUAGE AllowAmbiguousTypes #-}"
    , "{-# LANGUAGE DataKinds #-}"
    , "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE KindSignatures #-}"
    , "{-# LANGUAGE OverloadedStrings #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , "module " <> moduleName <> " where"
    , ""
    , "import qualified GHC.Types"
    , "import Data.Text (Text)"
    , "import qualified Data.Text.Internal"
    , "import Data.Monoid ((<>))"
    , ""
    , "static :: forall a. StaticFile a => Text"
    , "static = \"static/\" <> hashedPath @a" --TODO: Use obelisk-route to generate this in a more consistent way
    , ""
    , T.pack $ pprint decs
    ]

data StaticContext = StaticContext
  { _staticContext_className :: Name
  , _staticContext_methodName :: Name
  }

declareStatic :: FilePath -> Q [Dec]
declareStatic root = do
  paths <- runIO $ gatherHashedPaths root
  -- TODO: If https://ghc.haskell.org/trac/ghc/ticket/14623 is implemented, use
  -- qAddDependentFile to watch the directories as well as the files
  forM_ (Map.keys paths) $ \original -> do
    qAddDependentFile $ root </> original
  fmap toList $ execWriterT $ staticClassWithInstances paths

staticClassWithInstances :: Map FilePath FilePath -> WriterT (Seq Dec) Q ()
staticClassWithInstances paths = do
  ctx <- staticClass
  forM_ (Map.toList paths) $ \(original, hashed) -> do
    staticInstance ctx original hashed

staticClass :: WriterT (Seq Dec) Q StaticContext
staticClass = do
  let n x = Name (OccName x) NameS
      className = n "StaticFile"
      methodName = n "hashedPath"
      cls = ClassD [] className [KindedTV (n "s") (ConT ''Symbol)] [] [SigD methodName (ConT ''Text)]
  tell $ Seq.singleton cls
  return $ StaticContext
    { _staticContext_className = className
    , _staticContext_methodName = methodName
    }

staticInstance :: StaticContext -> FilePath -> FilePath -> WriterT (Seq Dec) Q ()
staticInstance ctx relativePath hashedPath = do
  let headType = ConT (_staticContext_className ctx) `AppT` LitT (StrTyLit relativePath)
      methodDec = ValD (VarP $ _staticContext_methodName ctx) (NormalB (LitE (StringL hashedPath))) []
  tell $ Seq.singleton $ InstanceD Nothing [] headType [methodDec]

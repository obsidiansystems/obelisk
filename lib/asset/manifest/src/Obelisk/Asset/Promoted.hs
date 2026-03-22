{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Obelisk.Asset.Promoted
  ( writeStaticProject
  , writeStaticModule
  , declareStatic
  , StaticConfig (..)
  ) where

import Obelisk.Asset.Gather

import Data.Foldable
import Language.Haskell.TH (pprint)
import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH.Datatype.TyVarBndr (kindedTVFlag)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Monad.Trans.Writer
import System.FilePath
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as L
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing)
import Obelisk.Asset.Cabal (writeCabalProject, SimplePkg(..))

#if !MIN_VERSION_base(4,21,0)
import GHC.TypeLits
#endif

data StaticConfig = StaticConfig
  { _staticConfig_packageName :: Text --TODO: Better type
  , _staticConfig_moduleName :: Text --TODO: Better type
  }

writeStaticProject :: Map FilePath FilePath -> FilePath -> StaticConfig -> IO ()
writeStaticProject paths target cfg = do
  let modName = _staticConfig_moduleName cfg
  modContents <- staticModuleFile modName paths
  writeCabalProject target $ SimplePkg
    { _simplePkg_name = _staticConfig_packageName cfg
    , _simplePkg_moduleName = _staticConfig_moduleName cfg
    , _simplePkg_moduleContents = modContents
    , _simplePkg_dependencies = ["base", "ghc-prim", "text"]
    }

-- | Write just the Haskell module file into an existing package directory,
-- without generating a .cabal file.
writeStaticModule :: Map FilePath FilePath -> FilePath -> Text -> IO ()
writeStaticModule paths target moduleName = do
  modContents <- staticModuleFile moduleName paths
  let (modName', moduleDirPath) = case L.uncons (reverse $ T.splitOn "." moduleName) of
        Nothing -> error $ "writeStaticModule: invalid module name " <> T.unpack moduleName
        Just (name, parents) -> (name, target </> "src" </> T.unpack (T.intercalate "/" $ reverse parents))
  createDirectoryIfMissing True moduleDirPath
  T.writeFile (moduleDirPath </> T.unpack modName' <.> "hs") modContents

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
    , "static = \"static/\" <> staticPath @a" --TODO: Use obelisk-route to generate this in a more consistent way
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
      methodName = n "staticPath"
#if MIN_VERSION_base(4,21,0)
      -- GHC >= 9.14 pprint emits GHC.Internal.Types.Symbol; use explicit GHC.Types name
      symbol = Name (OccName "Symbol") (NameQ (ModName "GHC.Types"))
#else
      symbol = ''Symbol
#endif
      cls = ClassD [] className [kindedTVFlag (n "s") breq (ConT symbol)] [] [SigD methodName (ConT ''Text)]

-- Can replace with Language.Haskell.TH.Datatype.TyVarBndr.BndrReq once support is dropped for th-abstractions < 0.6
#if MIN_VERSION_template_haskell(2,21,0)
      breq = BndrReq
#else
      breq = ()
#endif

  tell $ Seq.singleton cls
  return $ StaticContext
    { _staticContext_className = className
    , _staticContext_methodName = methodName
    }

staticInstance :: StaticContext -> FilePath -> FilePath -> WriterT (Seq Dec) Q ()
staticInstance ctx relativePath staticPath = do
  let headType = ConT (_staticContext_className ctx) `AppT` LitT (StrTyLit relativePath)
      methodDec = ValD (VarP $ _staticContext_methodName ctx) (NormalB (LitE (StringL staticPath))) []
  tell $ Seq.singleton $ InstanceD Nothing [] headType [methodDec]

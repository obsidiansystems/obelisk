module Obelisk.ExecutableConfig (get, getFrontendConfigs) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Maybe (maybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
import System.FilePath.Posix ((</>))

import Obelisk.ExecutableConfig.Internal.AssetManager

get :: Text -> IO (Maybe Text)
get name = bracket getAssets freeAssetManager $ \mgrObj -> do
  mgr <- assetManagerFromJava mgrObj
  getFromMgr mgr (withCString . T.unpack) name

getFromMgr mgr withCS name = do
  let open = do
        a <- withCS name $ \fn ->
          assetManager_open mgr fn 3
        return $ if unAAsset a == nullPtr
          then Nothing
          else Just a
      close = mapM_ asset_close
  bracket open close $ mapM $ \asset -> do
    b <- asset_getBuffer asset
    l <- asset_getLength asset
    fmap T.decodeUtf8 $ BS.packCStringLen (b, fromIntegral l)

getFrontendConfigs :: IO [(Text, Text)]
getFrontendConfigs = do
  cfgC <- getConfigs $ T.pack "config/common"
  cfgF <- getConfigs $ T.pack "config/frontend"
  return $ sortOn fst $ cfgC <> cfgF

getConfigs :: Text -> IO [(Text, Text)]
getConfigs fp = bracket getAssets freeAssetManager $ \mgrObj -> do
  mgr <- assetManagerFromJava mgrObj
  let openDir = do
        d <- withCString (T.unpack fp) $ \fn ->
          assetManager_openDir mgr fn
        return $ if unAAssetDir d == nullPtr
          then Nothing
          else Just d
      closeDir = mapM_ assetDir_close
  bracket openDir closeDir $ maybe (pure []) $ \assetDir -> do
    let go :: ([(Text,Text)] -> [(Text, Text)]) -> IO ([(Text, Text)] -> [(Text, Text)])
        go f = do
          next <- assetDir_getNextFileName assetDir
          if next == nullPtr
            then return f
            else do
              v <- getFromMgr mgr (flip id) next
              k <- fmap T.decodeUtf8 $ BS.packCString next
              go $ case (,) <$> Just k <*> v of
                Just p -> f . (p:)
                Nothing -> f
    flip id [] <$> go id

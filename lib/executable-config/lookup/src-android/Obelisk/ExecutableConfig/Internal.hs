{-# LANGUAGE LambdaCase #-}
module Obelisk.ExecutableConfig.Internal where

import Control.Exception (bracket)
import Control.Monad (forM)
import qualified Data.ByteString as BS
import Data.List (sortOn)
import Data.Maybe (catMaybes, maybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign.C.String (withCString)
import Foreign.Ptr (nullPtr)
import System.FilePath.Posix ((</>))

import Obelisk.ExecutableConfig.Internal.AssetManager

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
getFrontendConfigs
  = fmap (sortOn fst)
  $ getConfigs
  $ \fp -> BS.isPrefixOf (BS.pack $ fmap (fromIntegral . fromEnum) "config/common/") fp
        || BS.isPrefixOf (BS.pack $ fmap (fromIntegral . fromEnum) "config/frontend/") fp

getConfigs :: (BS.ByteString -> Bool) -> IO [(Text, Text)]
getConfigs p = bracket getAssets freeAssetManager $ \mgrObj -> do
  mgr <- assetManagerFromJava mgrObj
  let openDir = do
        d <- withCString "config.files" $ \fn ->
          assetManager_open mgr fn 3
        return $ if unAAsset d == nullPtr
          then Nothing
          else Just d
      closeDir = mapM_ asset_close
  configPaths <- bracket openDir closeDir $ \case
    Just asset -> do
      b <- asset_getBuffer asset
      l <- asset_getLength asset
      lines0 <$> BS.packCStringLen (b, fromIntegral l)
    Nothing -> error "could not open configuration manifest 'config.files'"
  fmap catMaybes $ forM (filter p configPaths) $ \fp ->
    fmap (\v -> (T.decodeUtf8 fp,v)) <$> getFromMgr mgr BS.useAsCString fp

lines0 :: BS.ByteString -> [BS.ByteString]
lines0 ps
    | BS.null ps = []
    | otherwise = case BS.elemIndex 0 ps of
             Nothing -> [ps]
             Just n  -> BS.take n ps : lines0 (BS.drop (n+1) ps)

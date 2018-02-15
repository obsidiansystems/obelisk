{-# LANGUAGE ForeignFunctionInterface #-}

module Obelisk.ExecutableConfig.Internal.AssetManager where

#include <jni.h>
#include <android/asset_manager.h>
#include <android/asset_manager_jni.h>

import Data.Int
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

newtype JObject = JObject { unJObject :: Ptr JObject }

newtype AAssetManager = AAssetManager { unAAssetManager :: Ptr AAssetManager }

newtype AAsset = AAsset { unAAsset :: Ptr AAsset }

foreign import ccall "ExecutableConfig_getAssets" getAssets :: IO JObject

foreign import ccall "ExecutableConfig_aassetManagerFromJava" assetManagerFromJava :: JObject -> IO AAssetManager

foreign import ccall "ExecutableConfig_freeAssetManager" freeAssetManager :: JObject -> IO ()

foreign import ccall "AAssetManager_open" assetManager_open :: AAssetManager -> CString -> CInt -> IO AAsset

foreign import ccall "AAsset_getBuffer" asset_getBuffer :: AAsset -> IO (Ptr CChar)

foreign import ccall "AAsset_getLength" asset_getLength :: AAsset -> IO #{type off_t}

foreign import ccall "AAsset_close" asset_close :: AAsset -> IO ()

{-# LANGUAGE ForeignFunctionInterface #-}

module WasmMain where

import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom

import Language.Javascript.JSaddle.Wasm
import Foreign.StablePtr (StablePtr)
import Foreign.C.String (CString)

foreign export ccall hsAppInit :: IO (StablePtr HsEnv)

hsAppInit :: IO (StablePtr HsEnv)
hsAppInit = do
  let Right validFullEncoder = checkEncoder fullRouteEncoder
  jsaddleInit 0 $ runFrontend validFullEncoder frontend

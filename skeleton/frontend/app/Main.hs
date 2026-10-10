{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

import Obelisk.Frontend
import Reflex.Dom

import Common.Route

import Frontend



main :: IO ()
main = run $ runFrontend checkedFullRouteEncoder frontend

#if defined(wasm32_HOST_ARCH)
foreign export javascript "hs_start" main :: IO ()
#endif

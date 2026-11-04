{-# LANGUAGE CPP #-}

import Backend (run)

#if defined(MIN_VERSION_frontend_js)
import Frontend.Cross.Javascript ()
#elif defined(MIN_VERSION_frontend_wasm)
import Frontend.Cross.Wasm ()
#endif

main :: IO ()
main = run

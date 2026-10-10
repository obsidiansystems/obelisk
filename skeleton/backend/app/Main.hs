import Backend (run)

-- The Frontend import keeps the frontend dependency marked as used: under
-- the default flags it is the frontend-wasm/frontend-js wrapper package,
-- whose custom Setup.hs cross-compiles the frontend during this build.
import Frontend ()

main :: IO ()
main = run

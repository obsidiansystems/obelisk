import Frontend
import Reflex.Dom

import Obelisk.ExecutableConfig (get)

main :: IO ()
main = do
  route <- get
  mainWidget $ snd $ frontend route

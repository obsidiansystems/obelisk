import Reflex.Dom (mainWidget)

import Frontend (frontend)

main :: IO ()
main = mainWidget $ snd frontend

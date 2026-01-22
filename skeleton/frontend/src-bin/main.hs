import Common.Route.Checked
import Frontend
import Obelisk.Frontend
import Reflex.Dom

main :: IO ()
main = run $ runFrontend validFullEncoder frontend

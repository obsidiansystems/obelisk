import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

main :: IO ()
main = do
  configs <- Lookup.getConfigs
  let Right validFullEncoder = checkEncoder domainBFullRouteEncoder
  run $ runFrontend validFullEncoder frontendB

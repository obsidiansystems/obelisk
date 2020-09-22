import Frontend
import Common.Route
import Obelisk.Frontend
import Obelisk.Route.Frontend
import Reflex.Dom
import qualified Obelisk.ExecutableConfig.Lookup as Lookup

main :: IO ()
main = do
  configs <- Lookup.getConfigs
  let domains = getCheckedDomainConfig configs
  let Right validFullEncoder = checkEncoder $ fullRouteEncoder domains
  run $ runFrontend validFullEncoder frontend

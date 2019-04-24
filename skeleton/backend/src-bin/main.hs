import Backend
import Frontend
import Obelisk.Backend

main :: IO ()
main = do
  cfg <- obeliskSnapConfig
  _backend_runner backend cfg (serveBackend backend frontend)

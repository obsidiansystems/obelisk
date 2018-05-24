{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Obelisk.ExecutableConfig.Core where

import Control.Lens
import Control.Monad ((<=<))
import Control.Monad.Catch (MonadCatch, displayException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import qualified Data.Text.IO as T
import System.FilePath
import Text.URI (ParseException, URI, mkURI)
import Text.URI.Lens

-- Config: common/route
newtype CommonRoute = CommonRoute URI
  deriving (Eq, Show, Ord)

class ObeliskConfig a where
  readObeliskConfig :: (MonadIO m, MonadCatch m) => FilePath -> m (Either String a)

instance ObeliskConfig CommonRoute where
  readObeliskConfig p = fmap (validateRoute <=< bimap toErrorString CommonRoute) $ try $ liftIO $
    mkURI =<< T.readFile routeFile
    where
      routeFile = p </> "config" </> "common" </> "route"
      toErrorString = displayException :: ParseException -> String
      validateRoute uri = maybe (Left "No port") (const $ Right uri) $ getRoutePort uri

getRoutePort :: CommonRoute -> Maybe Word
getRoutePort (CommonRoute uri) = uri ^? uriAuthority . _Right . authPort . _Just

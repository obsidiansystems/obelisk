{-# LANGUAGE OverloadedStrings #-}
module Obelisk.ExecutableConfig.Inject where

import Control.Monad (mapM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Reflex.Dom hiding (value)
import System.Directory
import System.FilePath ((</>))

-- | Produces a @<script>@ tag containing the contents of the config item,
-- rendered as a 'ByteString'.
inject :: FilePath -> IO ByteString
inject key = do
  value <- T.readFile key
  fmap snd $ renderStatic $ injectPure (T.pack key) value

-- | Produces a @<script>@ tag with an @id@ attribute containing the key and
-- whose value is the provided configuration
injectPure :: DomBuilder t m => Text -> Text -> m ()
injectPure key value =
  let attrs = ("type" =: "text/plain" <> "id" =: ("config-" <> key))
  in elAttr "script" attrs $ text value

-- | Scans the "common" and "frontend" configuration folders and produces
-- injectable @<script>@ tags containing the configuration keys (filepaths) and
-- values.
injectExecutableConfigs :: (MonadIO m, DomBuilder t m) => m ()
injectExecutableConfigs = do
  cfgC <- getConfigs "config/common"
  cfgF <- getConfigs "config/frontend"
  mapM_ (uncurry injectPure) (cfgC <> cfgF)

getConfigs :: MonadIO m => FilePath -> m [(Text, Text)]
getConfigs fp = liftIO $ do
  dir <- doesDirectoryExist fp
  if dir
    then do
      ps <- listDirectory fp
      fmap concat $ mapM (\p -> getConfigs $ fp </> p) ps
    else do
      file <- doesFileExist fp
      if file
        then do
          f <- T.readFile fp
          return [(T.pack fp, f)]
        else return []

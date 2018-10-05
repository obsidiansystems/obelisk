{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import Reflex.Dom.Core
import Obelisk.ExecutableConfig (get)

import Common.Api
import Static

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = do
      mbaseUrl <- liftIO $ get "common/route"
      forM_ mbaseUrl $ \baseUrl ->
        elAttr "base" ("href" =: baseUrl) $ return ()
      el "title" $ text "Obelisk Minimal Example"
    body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      elAttr "img" ("src" =: static @"obelisk.jpg") blank

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Control.Monad.IO.Class
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Route

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      ev <- button "Start the timer"
      widgetHold_ blank $ ffor ev $ \_ -> do
        performEvent_ . fmap (liftIO . print) =<< tickLossyFromPostBuildTime 5
        -- in 'ob run', if I change this to 1 and save
        -- the old timer will keep ticking and printing after reload
        -- and (this part was fixed by jsaddle bump) clicking the button triggers an error
  }

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Frontend where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Constraint (Dict(..))
import Data.Constraint.Extras
import Data.Constraint.Extras.TH
import Data.Dependent.Sum (DSum(..), (==>))
import Data.Dependent.Map (DMap)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Functor.Misc
import Data.GADT.Compare.TH
import qualified Data.Dependent.Map as DMap
import Data.Functor.Identity
import Data.Time (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Reflex.Patch.DMapWithMove

import Common.Api
import Common.Route
import Obelisk.Generated.Static

tshow :: Show a => a -> Text
tshow = T.pack . show

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pure ()
    -- TODO lib/Frontend needs fixing for this to work
    -- el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = do
      text "Welcome to Obelisk!"
      el "p" $ text $ T.pack commonStuff
      (e, ()) <- elAttr' "img" ("src" =: static @"obelisk.jpg") blank
      clicks <- count $ domEvent Click e
      performEvent $ liftIO . print <$> updated clicks
      el "p" $ text "Clicks: " >> el "span" (display clicks) -- TODO multiple text bug

      timer <- tickLossy 1 =<< liftIO getCurrentTime
      seconds <- count timer
      el "p" $ display seconds
      text "-----------------------------------------------"

--      elAttr "article" ("style" =: "background: lightcoral") $ do
--        prerender
--          (prerender
--            (el "span" $ text "Server span")
--            (el "a" $ text "JS link"))
--          (el "a" $ text "JS link")

      pb <- getPostBuild

      elAttr "article" ("style" =: "background: moccasin") $ do
        widgetHold_ (el "div" $ text "first") $ ffor pb $ \() -> display seconds

--      elAttr "article" ("style" =: "background: lightgreen") $ do
--        widgetHold_ (do prerender (liftIO $ putStrLn "-------- server missiles") (liftIO $ putStrLn "-------- client missiles"); el "div" $ text "first") never

      elAttr "article" ("style" =: "background: paleturquoise") $ do
        dyn_ $ ffor clicks $ \x -> case x `mod` 2 of
          0 -> el "span" $ text "Even"
          1 -> el "span" $ text "Odd"

      elAttr "article" ("style" =: "background: cornflowerblue") $ do
        widgetHold (text "first") $ ffor (updated clicks) $ \x -> case x `mod` 2 of
          0 -> do
            dyn_ $ ffor seconds $ \x -> case x `mod` 2 of
              0 -> el "span" $ text "Even"
              1 -> text "Odd"
          1 -> el "span" $ text "Odd inner"

      -- These will be squashed into a single text node by the static renderer
--      text "Test"
--      text "Test"
--      text "Test"
--      text "Test"
--      text "Test"

      --traverseDMapWithKeyWithAdjust' :: (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> m (DMap k v', Event t (PatchDMap k v'))
--      elAttr "article" ("style" =: "background: thistle") $ do
--        (m', e') <- traverseDMapWithKeyWithAdjust (\k v -> has @Show k $ dkeyWidget k v) keyMap $ ffor (updated clicks) $ \_ -> case 0 of -- x -> case x `mod` 2 of
--          0 -> PatchDMap $ DMap.singleton Key_Int (ComposeMaybe $ Just $ Identity 10)
--          1 -> PatchDMap $ DMap.singleton Key_Bool (ComposeMaybe $ case x `mod` 3 of 0 -> Nothing; _ -> Just $ Identity True) <> DMap.singleton Key_Char (ComposeMaybe $ Just $ Identity (toEnum $ fromEnum 'A' + x))
        pure ()


--      elAttr "article" ("style" =: "background: thistle") $ do
--        (m', e') <- traverseDMapWithKeyWithAdjust (\k v -> has @Show k $ dkeyWidget k v) keyMap $
--          PatchDMap (DMap.singleton Key_Int (ComposeMaybe $ Just $ Identity 10)) <$ pb
--        pure ()

  {-
      elAttr "article" ("style" =: "background: maroon") $ do
        widgetHold (text "first") $ ffor pb $ \_ -> do
          (m', e') <- traverseDMapWithKeyWithAdjust (\k v -> has @Show k $ dkeyWidget k v) keyMap $ ffor (updated clicks) $ \x -> case x `mod` 2 of
            0 -> PatchDMap $ DMap.singleton Key_Int (ComposeMaybe $ Just $ Identity x)
            1 -> PatchDMap $ DMap.singleton Key_Bool (ComposeMaybe $ case x `mod` 3 of 0 -> Nothing; _ -> Just $ Identity True) <> DMap.singleton Key_Char (ComposeMaybe $ Just $ Identity (toEnum $ fromEnum 'A' + x))
          pure ()

      move <- button "Move"
      ins <- button "Insert"
      del <- button "Delete"

      --traverseDMapWithKeyWithAdjust' :: (forall a. k a -> v a -> m (v' a)) -> DMap k v -> Event t (PatchDMap k v) -> m (DMap k v', Event t (PatchDMap k v'))
      elAttr "article" ("style" =: "background: yellow") $ do
        (m', e') <- traverseDMapWithKeyWithAdjustWithMove (\k v -> has @Show k $ dkeyWidget k v) keyMap $ leftmost
          -- TODO: move causes JSException here and in Immediate builder
          [ ffor move $ \() -> moveDMapKey Key_Int2 Key_Int
          , ffor ins $ \() -> insertDMapKey Key_Int (Identity 4)
          , ffor del $ \() -> deleteDMapKey Key_Int
          ]
        pure ()

      elAttr "article" ("style" =: "background: cyan") $ do
        (m', e') <- traverseIntMapWithKeyWithAdjust (\k v -> intkeyWidget k v) intMap $ leftmost
          -- TODO: move causes JSException here and in Immediate builder
          [ ffor move $ \() -> PatchIntMap $ IntMap.singleton 1 (Just 'Z')
          , ffor ins $ \() -> PatchIntMap $ IntMap.singleton 5 (Just 'Z')
          , ffor del $ \() -> PatchIntMap $ IntMap.singleton 3 Nothing
          ]
        pure ()
  -}

--      el "footer" $ text "Footer"
      pure ()
  }

intkeyWidget :: (MonadHold t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, DomBuilder t m, MonadFix m, MonadIO m, MonadIO (Performable m)) => Int -> Char -> m ()
intkeyWidget k v = el "div" $ do
  el "span" (text $ T.pack $ show k)
  case k of
    5 -> do
      seconds <- count =<< tickLossy 1 =<< liftIO getCurrentTime
      text " -> "
      dyn_ $ ffor seconds $ el "span" . text . tshow
    _ -> text $ " -> " <> tshow v
  pure ()

intMap :: IntMap Char
intMap = IntMap.fromList
  [ (1, 'A')
  , (2, 'B')
  , (3, 'C')
  , (4, 'D')
  ]

dkeyWidget :: (MonadHold t m, PostBuild t m, PerformEvent t m, TriggerEvent t m, DomBuilder t m, Show a, MonadFix m, MonadIO m, MonadIO (Performable m)) => DKey a -> Identity a -> m (Identity a)
dkeyWidget k (Identity v) = el "div" $ do
  el "span" (text $ textKey k)
  case k of
    Key_Bool | v -> do
      seconds <- count =<< tickLossy 1 =<< liftIO getCurrentTime
      text " -> "
      dyn_ $ ffor seconds $ el "span" . text . tshow
    _ -> text $ " -> " <> tshow v
  pure $ Identity v

keyMap :: DMap DKey Identity
keyMap = DMap.fromList
  [ Key_Int ==> -1
  , Key_Int2 ==> -100
  , Key_Bool ==> False
  , Key_Char ==> 'A'
  ]

data DKey a where
  Key_Int :: DKey Int
  Key_Char :: DKey Char
  Key_Bool :: DKey Bool
  Key_Int2 :: DKey Int

instance ArgDict DKey where
  type ConstraintsFor DKey c = (c Int, c Char, c Bool)
  argDict = \case
    Key_Int -> Dict
    Key_Int2 -> Dict
    Key_Char -> Dict
    Key_Bool -> Dict

textKey :: DKey a -> Text
textKey = \case
  Key_Int -> "Key_Int"
  Key_Int2 -> "Key_Int2"
  Key_Char -> "Key_Char"
  Key_Bool -> "Key_Bool"

deriveGEq ''DKey
deriveGCompare ''DKey
--deriveArgDict ''DKey

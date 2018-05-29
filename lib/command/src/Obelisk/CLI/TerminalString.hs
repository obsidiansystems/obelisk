{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Types and functions dealing with strings to be printed on terminal.
module Obelisk.CLI.TerminalString
  ( TerminalString(..)
  , render
  , putStrWithSGR
  , getTerminalWidth
  , enquiryCode
  ) where

import Control.Monad (when)
import Control.Monad.Catch (bracket_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ANSI (Color, ColorIntensity (Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor),
                            SGR, setSGR, setSGRCode)
import qualified System.Console.Terminal.Size as TerminalSize

-- | Printable text on terminals
--
-- Represents text with an optional color code.
data TerminalString
  = TerminalString_Normal Text
  | TerminalString_Colorized Color Text
  deriving (Eq, Show, Ord)

printableLength :: [TerminalString] -> Int
printableLength = T.length . toText False

-- Render a list of TerminalString as Text that can be directly putStr'ed.
render
  :: Bool -- ^ with color
  -> Maybe Int -- ^ optionally, trim to maximum width
  -> [TerminalString]
  -> Text
render withColor w ts = trim w $ toText withColor ts
  where
    trim = \case
      Nothing -> id
      Just n -> \s -> if printableLength ts > n
        then T.take (n-3) s <> "..." <> T.pack resetCode
        else s

toText :: Bool -> [TerminalString] -> Text
toText withColor = mconcat . map (toText' withColor)

-- | Convert to Text, controlling whether colorization should happen.
toText' :: Bool -> TerminalString -> Text
toText' withColor = \case
  TerminalString_Normal s -> s
  TerminalString_Colorized c s -> if withColor then colorizeText c s else s

-- | Colorize the given text so that it is printed in color when using putStr.
colorizeText :: Color -> Text -> Text
colorizeText color s = mconcat
  [ T.pack $ setSGRCode [SetColor Foreground Vivid color]
  , s
  , T.pack resetCode
  ]

-- | Safely print the string with the given ANSI control codes, resetting in the end.
putStrWithSGR :: MonadIO m => [SGR] -> Bool -> Text -> m ()
putStrWithSGR sgr withNewLine s = liftIO $ bracket_ (setSGR sgr) reset $ T.putStr s
  where
    reset = setSGR [Reset] >> newline -- New line should come *after* reset (to reset cursor color).
    newline = when withNewLine $ T.putStrLn ""

-- | Code for https://en.wikipedia.org/wiki/Enquiry_character
enquiryCode :: String
enquiryCode = "\ENQ"

-- | Code to reset ANSI colors
resetCode :: String
resetCode = setSGRCode [Reset]

getTerminalWidth :: IO (Maybe Int)
getTerminalWidth = fmap TerminalSize.width <$> TerminalSize.size


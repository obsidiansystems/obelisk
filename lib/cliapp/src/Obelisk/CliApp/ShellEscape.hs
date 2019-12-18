module Obelisk.CliApp.ShellEscape
  ( escape
  , fromEscaped
  , toEscaped
  ) where

import Shell.Utility.Quote (minimal)

newtype ShellEscaped = ShellEscaped { fromEscaped :: String }

escape :: String -> String
escape = minimal

toEscaped :: String -> ShellEscaped
toEscaped = ShellEscape . escape

escapeCommand :: [String] -> ShellEscape
escapeCommand = ShellEscaped . unwords . fmap escape

{-# LANGUAGE OverloadedStrings #-}
module Obelisk.CliApp.Theme where

import Data.Text (Text)

data CliTheme = CliTheme
  { _cliTheme_done :: Text
  , _cliTheme_failed :: Text
  , _cliTheme_arrow :: Text
  , _cliTheme_spinner :: SpinnerTheme
  }

type SpinnerTheme = [Text]

unicodeTheme :: CliTheme
unicodeTheme = CliTheme
  { _cliTheme_done = "✔"
  , _cliTheme_failed = "✖"
  , _cliTheme_arrow = "⇾"
  , _cliTheme_spinner = ["◐", "◓", "◑", "◒"]
  }

noUnicodeTheme :: CliTheme
noUnicodeTheme = CliTheme
  { _cliTheme_done = "DONE"
  , _cliTheme_failed = "FAILED"
  , _cliTheme_arrow = "->"
  , _cliTheme_spinner = ["|", "/", "-", "\\"]
  }

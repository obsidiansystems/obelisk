-- | An extremely low-ambition version of an opaque 'Path' library.
-- Libraries exist with much stronger guarantees but few (if any) of them
-- capture the idea of "canonical" paths in the types, which is critical
-- to many algorithms. Also the "buy-in" necessary for these approaches
-- is very high. This module provides the a minimal amount of type-level
-- tagging to allow the programmer to keep tabs on what's going on with
-- paths while also adding very little friction to interact with
-- 'FilePath'-based standard tools.
module Obelisk.Command.Path (module Reexport) where

import Obelisk.Command.Path.Internal as Reexport hiding (Path (..))
import Obelisk.Command.Path.Internal as Reexport (Path, unPath)

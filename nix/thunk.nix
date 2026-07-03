# Resolve a nix-thunk directory to its source, transparently handling both
# states: packed (the directory holds github.json + thunk.nix, so import
# thunk.nix to fetch the pin) and unpacked (the directory is a checkout of
# the dependency itself, so use it directly). Equivalent to nix-thunk's
# thunkSource.
path:
if builtins.pathExists (path + "/thunk.nix")
then import (path + "/thunk.nix")
else path

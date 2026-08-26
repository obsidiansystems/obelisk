# obelisk's shared helpers: nix-haskell's prelude, and whatever obelisk adds
# to it.
#
# Example:
#
#   with (import ./nix/libs/prelude { inherit lib; });
#
#   recurse-for-derivations { wasm = <drv>; }
#   => { wasm = <drv>; recurseForDerivations = true; }
{ lib, inputs ? {} }:

let nix-haskell-src =
      if inputs ? nix-haskell
      then inputs.nix-haskell
      else ../../../deps/nix-haskell;

in import (nix-haskell-src + "/libs/prelude") { inherit lib; }

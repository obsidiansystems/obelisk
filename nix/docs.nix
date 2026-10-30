{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix { inherit system; }
, pkgs ? inputs.pkgs
}:

let nix-haskell-src = inputs.nix-haskell;

    eval = import (nix-haskell-src + "/eval.nix") { inherit system pkgs inputs; };

    options = (eval (import ./module.nix)).options;

in import (nix-haskell-src + "/docs.nix") {
  inherit pkgs;
  options = { obelisk = options.obelisk; };
}

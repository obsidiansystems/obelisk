{ system ? builtins.currentSystem
, inputs ? {}
, pkgs ?
    if inputs ? nixpkgs
    then import inputs.nixpkgs { inherit system; }
    else import ../deps/nix-haskell/pins/nixpkgs { inherit system; }
}:

let nix-haskell-src =
      if inputs ? nix-haskell
      then inputs.nix-haskell
      else ../deps/nix-haskell;

    eval = import (nix-haskell-src + "/eval.nix") { inherit system pkgs inputs; };

    options = (eval (import ./module.nix)).options;

in import (nix-haskell-src + "/docs.nix") {
  inherit pkgs;
  options = { obelisk = options.obelisk; };
}

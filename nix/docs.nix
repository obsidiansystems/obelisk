{ system ? builtins.currentSystem
, inputs ? {}
, pkgs ?
    if inputs ? nixpkgs
    then import inputs.nixpkgs { inherit system; }
    else import ((import ../deps/nix-haskell/thunk.nix) + "/pins/nixpkgs") { inherit system; }
}:

let nix-haskell-src =
      if inputs ? nix-haskell
      then inputs.nix-haskell
      else import ../deps/nix-haskell/thunk.nix;

    eval = import (nix-haskell-src + "/eval.nix") { inherit system pkgs inputs; };

    options = (eval (import ./module.nix)).options;

in import (nix-haskell-src + "/docs.nix") {
  inherit pkgs;
  options = { obelisk = options.obelisk; };
}

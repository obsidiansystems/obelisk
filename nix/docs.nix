{ system ? builtins.currentSystem
, pkgs ? import ../deps/nix-haskell/pins/nixpkgs { inherit system; }
}:

let eval = import ../deps/nix-haskell/eval.nix { inherit system pkgs; };

    options = (eval (import ./module.nix)).options;

in import ../deps/nix-haskell/docs.nix {
  inherit pkgs;
  options = { obelisk = options.obelisk; };
}

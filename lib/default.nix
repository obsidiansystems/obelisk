{ system ? builtins.currentSystem, inputs ? {} }:

let nix-haskell =
      if inputs ? nix-haskell
      then import inputs.nix-haskell { inherit system inputs; }
      else import ../deps/nix-haskell { inherit system; };

    project = nix-haskell (import ./project.nix);

in {
  inherit (project) config pkgs;

  haskell-nix = project.haskell-nix.project;
  nixpkgs = project.nixpkgs.project;
}

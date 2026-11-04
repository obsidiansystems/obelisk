{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix { inherit system; }
}:

let nix-haskell = import inputs.nix-haskell { inherit system inputs; };

    project = nix-haskell (import ./project.nix);

in {
  inherit (project) config pkgs;

  haskell-nix = project.haskell-nix.project;
  nixpkgs = project.nixpkgs.project;
}

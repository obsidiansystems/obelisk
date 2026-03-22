{ system }:

let lib = import ./lib.nix { inherit system; };

    nix-haskell = import ../deps/nix-haskell { inherit system; };

    module = import ./module.nix;

in lib // {
  inherit module;
  inherit (lib) extraCabalProject;

  project = userModule: (nix-haskell {
    imports = [
      module
      userModule
    ];
  }).haskell-nix.project;
}

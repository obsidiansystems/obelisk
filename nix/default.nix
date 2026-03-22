{ system }:

let lib = import ./lib.nix { inherit system; };

    nix-haskell = import ../deps/nix-haskell { inherit system; };

    module = import ./module.nix;

in lib // {
  inherit module;
  inherit (lib) extraCabalProject;

  project = userModule:
    let proj = (nix-haskell {
          imports = [
            module
            userModule
          ];
        }).haskell-nix.project;
    in proj // {
      exe = {
        wasm = (proj.override { obelisk.frontend.target = "wasm"; }).hsPkgs.backend.components.exes.backend;
        js = (proj.override { obelisk.frontend.target = "js"; }).hsPkgs.backend.components.exes.backend;
      };
    };
}

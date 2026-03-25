{ system, inputs ? {} }:

let pkgs =
      if inputs ? nixpkgs
      then import inputs.nixpkgs { inherit system; }
      else import ../deps/nix-haskell/pins/nixpkgs { inherit system; };

    nix-haskell =
      if inputs ? nix-haskell
      then import inputs.nix-haskell { inherit system pkgs inputs; }
      else import ../deps/nix-haskell { inherit system; };

    lib = import ./lib.nix { inherit system inputs pkgs; };

    module = import ./module.nix;

in lib // {
  inherit module;
  inherit (lib) extraCabalProject serverModule;

  project = userModule:
    let eval = nix-haskell {
          imports = [
            module
            userModule
          ];
        };
        proj = eval.haskell-nix.project;
        pkgs = eval.nixpkgs;
        serverExe = {
          wasm = lib.mkServerExe { inherit proj; target = "wasm"; };
          js = lib.mkServerExe { inherit proj; target = "js"; };
        };
    in proj // {
      config = eval.config;
      nixpkgs = eval.nixpkgs;

      exe = {
        wasm = (proj.override { obelisk.frontend.target = "wasm"; }).hsPkgs.backend.components.exes.backend;
        js = (proj.override { obelisk.frontend.target = "js"; }).hsPkgs.backend.components.exes.backend;
      };

      inherit serverExe;

      containerImage = {
        wasm = lib.mkContainerImage { inherit proj; target = "wasm"; };
        js = lib.mkContainerImage { inherit proj; target = "js"; };
      };

      server = { exe ? serverExe.wasm, ... }@args:
        let nixos = import (pkgs.path + /nixos);
        in nixos {
          inherit system;
          configuration = {
            imports = [ lib.serverModule ];
            services.obelisk = { enable = true; } // args;
          };
        };
    };
}

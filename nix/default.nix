{ system ? builtins.currentSystem, inputs ? {} }:

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

        outputs = { proj, targets, defaultTarget }:
          let serverExe = pkgs.lib.genAttrs targets (target:
                lib.mkServerExe { inherit proj target; });
          in proj // {
            exe = pkgs.lib.genAttrs targets (target:
              lib.perDriver proj.config lib.backendExe
                (proj.override { obelisk.frontend.target = target; }));

            inherit serverExe;

            containerImage = pkgs.lib.genAttrs targets (target:
              lib.mkContainerImage { inherit proj target; });

            server = { exe ? serverExe.${defaultTarget}, ... }@args:
              let nixos = import (eval.pkgs.path + /nixos);
              in nixos {
                inherit system;
                configuration = {
                  imports = [ lib.serverModule ];
                  services.obelisk = { enable = true; } // args;
                };
              };
          };

        # The nixpkgs driver has no wasm compiler of its own. It builds the
        # target only where the project supplies one.
        nixpkgsTargets = [ "js" ]
          ++ pkgs.lib.optional (eval.config.nixpkgs.pkgsCross ? wasi32) "wasm";

    in {
      config = eval.config;
      pkgs = eval.pkgs;
      inherit (lib) serverModule;

      haskell-nix = outputs {
        proj = eval.haskell-nix.project;
        targets = [ "wasm" "js" ];
        defaultTarget = "wasm";
      };

      nixpkgs = outputs {
        proj = eval.nixpkgs.project.override { obelisk.driver = "nixpkgs"; };
        targets = nixpkgsTargets;
        defaultTarget = "js";
      };
    };
}

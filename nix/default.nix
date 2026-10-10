{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix
, pkgs ? import inputs.nixpkgs { inherit system; }
}:

let nix-haskell = import inputs.nix-haskell { inherit system pkgs inputs; };

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

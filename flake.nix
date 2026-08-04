{
  inputs = {
    # Dependencies are git submodules under deps/; this makes nix fetch them
    # automatically when the flake is fetched over git (Nix 2.27+; on older
    # Nix, add ?submodules=1 to the flake URL).
    self.submodules = true;

    nix-haskell.url = ./deps/nix-haskell;
    reflex-dom.url = ./deps/reflex-dom;

    nixpkgs.follows = "nix-haskell/nixpkgs";
    haskell-nix.follows = "nix-haskell/haskell-nix";

    flake-compat.follows = "nix-haskell/flake-compat";
  };

  outputs = inputs@{ self, ... }:
    let nixpkgs = if inputs ? "nixpkgs" then inputs.nixpkgs else builtins.getFlake "nixpkgs";
        eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in {
      lib = eachSystem (system:
        import ./nix { inherit system inputs; }
      );

      packages = eachSystem (system: {
        docs = (import ./nix/docs.nix { inherit system inputs; }).md;
        release = (import ./release.nix { inherit system inputs; }).all;
      });

      devShells = eachSystem (system:
        let shells = import ./shell.nix { inherit system inputs; };
        in {
          default = shells.haskell-nix;
          inherit (shells) nixpkgs;
        });

      # Scaffold a new project without cloning obelisk by hand:
      #   nix run github:obsidiansystems/obelisk#init -- my-app
      # The generated project gets obelisk as a git submodule at deps/obelisk,
      # pinned to the exact revision this flake was fetched at.
      apps = eachSystem (system:
        let pkgs = nixpkgs.legacyPackages.${system};
            ob-init = pkgs.writeShellApplication {
              name = "ob-init";
              runtimeInputs = [ pkgs.git ];
              text = builtins.readFile ./scripts/ob-init;
            };
            init = pkgs.writeShellApplication {
              name = "obelisk-init";
              runtimeInputs = [ pkgs.git ];
              text = ''
                export OBELISK_SKELETON="''${OBELISK_SKELETON:-${self}/skeleton}"
                export OBELISK_PIN_REV="''${OBELISK_PIN_REV:-${self.rev or ""}}"
                if [ -z "$OBELISK_PIN_REV" ]; then
                  echo "obelisk#init: this flake was fetched without a revision" >&2
                  echo "(dirty tree, or a path: URL). The scaffold would end up" >&2
                  echo "pinned to a /nix/store snapshot instead of a submodule." >&2
                  echo "Run ob-init from an obelisk checkout, or pass --rev." >&2
                  exit 1
                fi
                exec ${ob-init}/bin/ob-init "$@"
              '';
            };
        in {
          init = { type = "app"; program = "${init}/bin/obelisk-init"; };
        });
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.nixos.org"
      "https://nixcache.reflex-frp.org"
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" # reflex-frp
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = "true";
  };
}

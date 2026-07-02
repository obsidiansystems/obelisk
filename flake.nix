{
  # Dependencies are pinned as nix-thunks under deps/ (see deps/*/github.json),
  # not git submodules, so no `?submodules=1` / `--recursive` is needed. The nix
  # code (nix/*.nix) imports them directly; `nixpkgs` for the flake outputs comes
  # from the nix-haskell thunk's pins.
  outputs = { self, ... }:
    let systems = [ "x86_64-linux" "aarch64-linux" ];
        # genAttrs without importing nixpkgs: the top-level import took no
        # `system`, which broke pure evaluation (nix flake show, nix run).
        eachSystem = f: builtins.listToAttrs
          (map (system: { name = system; value = f system; }) systems);
        pkgsFor = system:
          import ((import ./deps/nix-haskell/thunk.nix) + "/pins/nixpkgs") { inherit system; };
    in {
      lib = eachSystem (system:
        import ./nix { inherit system; }
      );

      packages = eachSystem (system: {
        docs = (import ./nix/docs.nix { inherit system; }).md;
        release = import ./release.nix { inherit system; };
      });

      devShells = eachSystem (system: {
        default = (import ./skeleton { inherit system; }).shell;
      });

      # Scaffold a new project without cloning obelisk:
      #   nix run github:obsidiansystems/obelisk#init -- my-app
      # The generated project pins deps/obelisk to the exact revision this
      # flake was fetched at (as a nix-thunk).
      apps = eachSystem (system:
        let pkgs = pkgsFor system;
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
                export OBELISK_PIN_SHA256="''${OBELISK_PIN_SHA256:-${self.narHash}}"
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

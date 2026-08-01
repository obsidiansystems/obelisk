{
  inputs = {
    # obelisk lives at deps/obelisk as a git submodule, so the flake commands
    # and plain nix-shell/nix-build (which go through default.nix) build from
    # the same source. That makes obelisk part of this project's submodules:
    # Nix 2.27+ fetches them from the line below, on older Nix add
    # ?submodules=1 to this project's flake URL.
    self.submodules = true;

    obelisk.url = ./deps/obelisk;

    nixpkgs.follows = "obelisk/nixpkgs";
  };

  outputs = inputs@{ self, ... }:
    let nixpkgs = if inputs ? "nixpkgs" then inputs.nixpkgs else builtins.getFlake "nixpkgs";
        eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in {
      legacyPackages = eachSystem (system: {
        default = import ./default.nix { inherit system inputs; };
      });

      devShells = eachSystem (system: {
        default = import ./shell.nix { inherit system inputs; };
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

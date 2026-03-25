{
  inputs = {
    nix-haskell.url = "git+file:./deps/nix-haskell";
    nixpkgs.follows = "nix-haskell/nixpkgs";
    haskell-nix.follows = "nix-haskell/haskell-nix";
    reflex-platform.follows = "nix-haskell/reflex-platform";
  };

  outputs = inputs@{ self, nixpkgs, ... }:
    let eachSystem = nixpkgs.lib.genAttrs
          [ "x86_64-linux"
            "aarch64-linux"
          ];
    in {
      lib = eachSystem (system:
        import ./nix { inherit system inputs; }
      );

      packages = eachSystem (system: {
        docs = (import ./nix/docs.nix { inherit system inputs; }).docs;
        release = import ./release.nix { inherit system inputs; };
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

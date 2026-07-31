{
  inputs = {
    # ob-init rewrites this to pin the obelisk revision a project was
    # scaffolded from. ?submodules=1 makes the fetch include obelisk's deps/
    # submodules on any Nix version (on 2.27+ it is redundant with obelisk's
    # inputs.self.submodules = true).
    obelisk.url = "github:obsidiansystems/obelisk?submodules=1";

    nixpkgs.follows = "obelisk/nixpkgs";
  };

  outputs = inputs@{ self, obelisk, nixpkgs }:
    let eachSystem = nixpkgs.lib.genAttrs nixpkgs.lib.systems.flakeExposed;
    in {
      legacyPackages = eachSystem (system:
        import ./default.nix { inherit system inputs; }
      );

      devShells = eachSystem (system: {
        default = (import ./default.nix { inherit system inputs; }).shell;
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

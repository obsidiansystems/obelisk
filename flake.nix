{
  # Dependencies are pinned as nix-thunks under deps/ (see deps/*/github.json),
  # not git submodules — so no `?submodules=1` / `--recursive` is needed. The nix
  # code (nix/*.nix) imports them directly; `nixpkgs` for the flake outputs comes
  # from the nix-haskell thunk's pins.
  outputs = { self, ... }:
    let nixpkgs = import ((import ./deps/nix-haskell/thunk.nix) + "/pins/nixpkgs") {};
        eachSystem = nixpkgs.lib.genAttrs
          [ "x86_64-linux"
            "aarch64-linux"
          ];
    in {
      lib = eachSystem (system:
        import ./nix { inherit system; }
      );

      packages = eachSystem (system: {
        docs = (import ./nix/docs.nix { inherit system; }).docs;
        release = import ./release.nix { inherit system; };
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

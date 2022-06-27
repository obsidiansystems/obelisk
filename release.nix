{ supportedSystems ? [ "x86_64-linux" ]
}:
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; version = "ghc-8.10.7"; };
  test = import ./all-tests.nix { inherit supportedSystems; };

  inherit (build) metaCache;
}

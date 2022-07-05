{ supportedSystems ? [ "x86_64-linux" ]
}:
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; version = "ghc-8.6.5"; };
  test = import ./all-tests.nix { inherit supportedSystems; };

  inherit (build) metaCache;
}

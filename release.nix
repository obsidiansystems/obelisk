{ supportedSystems ? [ "x86_64-linux" ]
}:
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
  beta = (import ./skeleton { system = "aarch64-darwin"; }).shells.ghc;

  inherit (build) metaCache;
}

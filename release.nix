{ supportedSystems ? [ "x86_64-linux" ]
}:
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
  beta = {
    skeleton = (import ./skeleton { system = "aarch64-darwin"; }).shells.ghc;
    skeletonExe = (import ./skeleton { system = "aarch64-darwin"; }).exe;
    command = (import ./.{ system = "aarch64-darwin"; }).command;
  };

  inherit (build) metaCache;
}

{ supportedSystems ? [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ]
}:
rec {
  recurseForDerivations = true;

  # build = import ./all-builds.nix { inherit supportedSystems; };
  # test = import ./all-tests.nix { inherit supportedSystems; };
  # inherit (build) metaCache;

  # TODO: re-enable metaCache instead
  inherit (import ./skeleton {}) exe;
  inherit ((import ./. {}).marsObelisk.hsPkgs) obelisk-selftest;
}

{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  obelisk = import ./. {};
  pkgs = obelisk.nixpkgs;
  recurse = x: x // { recurseForDerivations = true; };
in
  recurse (pkgs.lib.genAttrs supportedSystems (system:
    let
      skeleton = import ./skeleton { inherit system; };
    in with skeleton; recurse ({
      inherit exe;
      inherit (obelisk.marsObelisk.hsPkgs) obelisk-selftest;
    } // pkgs.lib.optionalAttrs (system == "x86_64-linux") {
      android-app = recurse android.app;
    } // pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
      ios-app = recurse ios.app;
    })))

/*
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
  inherit (build) metaCache;
}
*/

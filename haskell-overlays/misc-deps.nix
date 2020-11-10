{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  regex-base = self.callHackage "regex-base" "0.94.0.0" {};
  regex-posix = self.callHackage "regex-posix" "0.96.0.0" {};
  regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" {};
  test-framework = haskellLib.dontCheck (self.callHackage "test-framework" "0.8.2.0" {});
  hnix = haskellLib.dontCheck (self.callHackage "hnix" "0.8.0" {});
  hnix-store-core = self.callHackage "hnix-store-core" "0.2.0.0" {};

  aeson-gadt-th = self.callHackage "aeson-gadt-th" "0.2.4" {};

  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) {};
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  shelly = self.callHackage "shelly" "1.9.0" {};
}

{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  hnix = haskellLib.dontCheck (haskellLib.doJailbreak (self.callCabal2nix "hnix" (hackGet ../dep/hnix) {}));
  hnix-store-core = self.callHackage "hnix-store-core" "0.1.0.0" {};

  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) {};
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  shelly = self.callHackage "shelly" "1.9.0" {};
}

{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  # Actually broken in current nixpkgs master due to MonadFail changes
  git = haskellLib.markUnbroken super.git;

  # hpack requires cabal >= 3.0 but the ghc865 package set builds it with 2.4 by default
  hpack = super.hpack.overrideScope (self: super: { Cabal = self.Cabal_3_2_0_0; });

  # These versions work with both the ghc865 and ghc8107 package sets
  universe = self.callHackage "universe" "1.2" {};
  universe-instances-extended = self.callHackage "universe-instances-extended" "1.1.1" {};

  regex-base = self.callHackage "regex-base" "0.94.0.0" {};
  regex-posix = self.callHackage "regex-posix" "0.96.0.0" {};
  regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" {};
  test-framework = haskellLib.dontCheck (self.callHackage "test-framework" "0.8.2.0" {});
  hnix = haskellLib.dontCheck super.hnix;
  hnix-store-core = haskellLib.dontCheck super.hnix-store-core;
  hnix-store = haskellLib.dontCheck super.hnix-store;

  aeson-gadt-th = self.callHackage "aeson-gadt-th" "0.2.4" {};

  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) {};
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  logging-effect = self.callCabal2nix "logging-effect" (hackGet ../dep/logging-effect) {};
  resourcet = self.callHackage "resourcet" "1.2.4.2" {};
  unliftio-core = self.callHackage "unliftio-core" "0.2.0.1" {};
  shelly = self.callHackage "shelly" "1.9.0" {};
  monad-logger = self.callHackage "monad-logger" "0.3.36" {};
  nix-thunk = (import ../dep/nix-thunk {}).makeRunnableNixThunk (self.callCabal2nix "nix-thunk" (hackGet ../dep/nix-thunk) {});
  cli-extras = self.callCabal2nix "cli-extras" (hackGet ../dep/cli-extras) {};
  cli-git = haskellLib.overrideCabal (self.callCabal2nix "cli-git" (hackGet ../dep/cli-git) {}) {
    librarySystemDepends = with pkgs; [
      git
    ];
  };
  cli-nix = self.callCabal2nix "cli-nix" (hackGet ../dep/cli-nix) {};
}

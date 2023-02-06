{ hackGet, __useNewerCompiler ? false }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) { };
  haskellLib = pkgs.haskell.lib;
  mkVersionset = v: p: q: if __useNewerCompiler then q else p;
in

rec {
  # hpack requires cabal >= 3.0 but the ghc865 package set builds it with 2.4 by default
  hpack = if __useNewerCompiler
    then super.hpack
    else super.hpack.overrideScope (self: super: { Cabal = self.Cabal_3_2_1_0; });

  # These versions work with both the ghc865 and ghc8107 package sets
  git = self.callCabal2nix "git" (hackGet ../dep/hs-git) { };
  http-link-header = haskellLib.doJailbreak super.http-link-header;
  universe-base-810 = haskellLib.doJailbreak (self.callHackage "universe-base" "1.1" {});
  universe-dependent-sum-810 = self.callHackage "universe-dependent-sum" "1.3" {};
  universe-some-810 = haskellLib.dontHaddock (haskellLib.appendBuildFlags (haskellLib.doJailbreak (self.callHackage "universe-some" "1.2" { })) [ "--ghc-option=-Wno-inferred-safe-imports" "--ghc-option=-Wno-missing-safe-haskell-mode" ]);

  beam-migrate = self.callHackage "beam-migrate" "0.5.1.2" {};

  universe-810 = self.callHackage "universe" "1.2" {};
  universe-instances-extended-810 = self.callHackage "universe-instances-extended" "1.1.1" {};
  universe-reverse-instances-810 = self.callHackage "universe-reverse-instances" "1.1" {};

  # We use our fork of hnix which has some compatibility patches on top of 0.12 from hackage
  hnix = haskellLib.dontHaddock (haskellLib.dontCheck (self.callCabal2nix "hnix" (hackGet ../dep/hnix) {}));

  universe-86 = haskellLib.dontCheck (self.callHackage "universe" "1.2" {});
  universe-instances-extended-86 = self.callHackage "universe-instances-extended" "1.1.1" {};

  universe = mkVersionset __useNewerCompiler universe-86 universe-810;
  universe-instances-extended = mkVersionset __useNewerCompiler universe-instances-extended-86 universe-instances-extended-810;
  universe-reverse-instances = mkVersionset __useNewerCompiler super.universe-reverse-instances universe-reverse-instances-810;
  universe-base = haskellLib.dontCheck (mkVersionset __useNewerCompiler super.universe-base universe-base-810);
  universe-dependent-sum = mkVersionset __useNewerCompiler super.universe-dependent-sum universe-dependent-sum-810;
  universe-some-86 = self.callHackage "universe-some" "1.2" {};
  universe-some = mkVersionset __useNewerCompiler universe-some-86 universe-some-810;

  hnix-store-core = haskellLib.dontCheck super.hnix-store-core;
  hnix-store = haskellLib.dontCheck super.hnix-store;

  nix-derivation = haskellLib.doJailbreak super.nix-derivation;
  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) { };

  shelly = self.callHackage "shelly" "1.9.0" { };
  # version >= 0.2.5.2 has a Cabal version of 3.0, which nix doesn't like
  vector-binary-instances = self.callHackage "vector-binary-instances" "0.2.5.1" {};
  modern-uri = if __useNewerCompiler
    then super.modern-uri
    else haskellLib.doJailbreak super.modern-uri;
  neat-interpolation = haskellLib.doJailbreak super.neat-interpolation;
  nix-thunk = (import ../dep/nix-thunk { }).makeRunnableNixThunk (haskellLib.doJailbreak (self.callCabal2nix "nix-thunk" (hackGet ../dep/nix-thunk) { }));

  # aeson 2.0 & attoparsec 0.14 support overrides
  binary-instances = haskellLib.doJailbreak (self.callHackage "binary-instances" "1.0.3" {});
  binary-orphans = self.callHackage "binary-orphans" "1.0.3" {};
  semialign-indexed = haskellLib.doJailbreak (self.callHackage "semialign-indexed" "1.2" {});
  io-streams = self.callHackage "io-streams" "1.5.2.1" {};
  io-streams-haproxy = self.callHackage "io-streams-haproxy" "1.0.1.0" {};
  snap-core = self.callHackage "snap-core" "1.0.5.0" {};
  snap-server = haskellLib.doJailbreak super.snap-server;
  cborg = self.callHackage "cborg" "0.2.8.0" {};
  heist = haskellLib.dontCheck (self.callHackage "heist" "1.1.1.0" {});
  github = self.callHackage "github" "0.28.0.1" {};
  http-streams = self.callHackage "http-streams" "0.8.9.6" {};
  http-common = self.callHackage "http-common" "0.8.3.4" {};

  cli-extras = haskellLib.doJailbreak (self.callCabal2nix "cli-extras" (hackGet ../dep/cli-extras) { });
  cli-git = haskellLib.overrideCabal (self.callCabal2nix "cli-git" (hackGet ../dep/cli-git) { }) {
    librarySystemDepends = with pkgs; [
      gitMinimal
    ];
  };
  cli-nix = haskellLib.overrideCabal (self.callCabal2nix "cli-nix" (hackGet ../dep/cli-nix) { }) {
    librarySystemDepends = with pkgs; [
      gitMinimal
      nix
      nix-prefetch-git
    ];
  };

  haddock-library = haskellLib.doJailbreak (self.callHackage "haddock-library" "1.10.0" {});
}

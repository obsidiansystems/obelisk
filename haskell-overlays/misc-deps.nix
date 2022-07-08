{ hackGet, __useNewerCompiler ? false }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) { };
  haskellLib = pkgs.haskell.lib;
  mkVersionset = v: p: q: if __useNewerCompiler then q else p;
in

rec {
  # Actually broken in current nixpkgs master due to MonadFail changes
  # git = haskellLib.markUnbroken super.git;

  # hpack requires cabal >= 3.0 but the ghc865 package set builds it with 2.4 by default
  hpack = super.hpack.overrideScope (self: super: { Cabal = self.Cabal_3_2_0_0; });
  cabal-install = null;
  # These versions work with both the ghc865 and ghc8107 package sets
  git = self.callCabal2nix "git" (hackGet ../dep/hs-git) { };
  # hdevtools = haskellLib.markUnbroken super.hdevtools;
  # reflex-ghci = haskellLib.markUnbroken super.reflex-ghci;
  # reflex-process = haskellLib.markUnbroken super.reflex-process;
  # reflex-vty = haskellLib.markUnbroken super.reflex-vty;
  # reflex-fsnotify = haskellLib.markUnbroken super.reflex-fsnotify;
  universe-base-810 = haskellLib.doJailbreak (self.callHackage "universe-base" "1.1.3" {});
  universe-dependent-sum-810 = self.callHackage "universe-dependent-sum" "1.3" {};
  universe-some-810 = haskellLib.dontHaddock (haskellLib.appendBuildFlags (haskellLib.doJailbreak (self.callHackage "universe-some" "1.2.1" { })) [ "--ghc-option=-Wno-inferred-safe-imports" "--ghc-option=-Wno-missing-safe-haskell-mode" ]);

  stylish-haskell = null; # FIXME

  universe-810 = self.callHackage "universe" "1.2.2" {};
  universe-instances-extended-810 = self.callHackage "universe-instances-extended" "1.1.3" {};
  universe-reverse-instances-810 = self.callHackage "universe-reverse-instances" "1.1.1" {};
       
  hnix = haskellLib.overrideCabal super.hnix (drv: {
        jailbreak = true;
        preBuild = ''
        substituteInPlace src/Nix/Expr/Types.hs --replace "instance Hashable1 NonEmpty" ""
  '';});
      
  universe-86 = haskellLib.dontCheck (self.callHackage "universe" "1.2" {});
  universe-instances-extended-86 = self.callHackage "universe-instances-extended" "1.1.1" {};
  hnix-86 = haskellLib.dontCheck super.hnix;
    
  universe = mkVersionset __useNewerCompiler universe-86 universe-810;
  universe-instances-extended = mkVersionset __useNewerCompiler universe-instances-extended-86 universe-instances-extended-810;
  universe-reverse-instances = mkVersionset __useNewerCompiler super.universe-reverse-instances universe-reverse-instances-810;
  #hnix = mkVersionset version hnix-86 hnix-810;
  universe-base = haskellLib.dontCheck (mkVersionset __useNewerCompiler super.universe-base universe-base-810);
  universe-dependent-sum = mkVersionset __useNewerCompiler super.universe-dependent-sum universe-dependent-sum-810;
  universe-some-86 = self.callHackage "universe-some" "1.2" {};
  universe-some = mkVersionset __useNewerCompiler universe-some-86 universe-some-810;
  
  #th-abstraction-86 = self.callHackage "th-abstraction" "0.3.0.0" {};
  #th-abstraction-810 = self.callHackage "th-abstraction" "0.4.3.0" {};
  #th-abstraction = mkVersionset version th-abstraction-86 th-abstraction-810;
  #bifunctors = self.callHackage "bifunctors" "5.5.11" { th-abstraction = th-abstraction-new; };
  #template-haskell = self.callHackage "template-haskell" "2.14.0.0" {};
  regex-base = self.callHackage "regex-base" "0.94.0.0" { };
  regex-posix = self.callHackage "regex-posix" "0.96.0.0" { };
  regex-tdfa = self.callHackage "regex-tdfa" "1.3.1.0" { };
  test-framework = haskellLib.dontCheck (self.callHackage "test-framework" "0.8.2.0" { });
  
  hnix-store-core = haskellLib.dontCheck super.hnix-store-core;
  hnix-store = haskellLib.dontCheck super.hnix-store;
  
  # https://github.com/haskell/hackage-security/issues/247
  hackage-security = haskellLib.dontCheck super.hackage-security; # only tests use aeson and are not compat with 1.5;
  heist = haskellLib.dontCheck (haskellLib.doJailbreak super.heist); # aeson 1.5 bump
  aeson-gadt-th = haskellLib.doJailbreak super.aeson-gadt-th; # requires aeson 1.5 for ghc8.10 support?
  deriving-compat = self.callHackage "deriving-compat" "0.6" { };
  #deriving-compat = mkVersionset version super.deriving-compat deriving-compat-810;
  http-api-data = haskellLib.doJailbreak super.http-api-data;
  nix-derivation = haskellLib.doJailbreak super.nix-derivation;
  algebraic-graphs = haskellLib.doJailbreak super.algebraic-graphs;
  snap = haskellLib.doJailbreak super.snap;
  ghcid = self.callCabal2nix "ghcid" (hackGet ../dep/ghcid) { };
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) { });

  logging-effect = self.callCabal2nix "logging-effect" (hackGet ../dep/logging-effect) { };
  resourcet = self.callHackage "resourcet" "1.2.4.2" { };
  unliftio-core = self.callHackage "unliftio-core" "0.2.0.1" { };
  shelly = self.callHackage "shelly" "1.9.0" { };
  monad-logger = self.callHackage "monad-logger" "0.3.36" { };
}

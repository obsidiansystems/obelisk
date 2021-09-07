{ hackGet }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
in

{
  git = self.callCabal2nix "git" (hackGet ../dep/hs-git) {};
  # hdevtools = haskellLib.markUnbroken super.hdevtools;
  # reflex-ghci = haskellLib.markUnbroken super.reflex-ghci;
  # reflex-process = haskellLib.markUnbroken super.reflex-process;
  # reflex-vty = haskellLib.markUnbroken super.reflex-vty;
  # reflex-fsnotify = haskellLib.markUnbroken super.reflex-fsnotify;
  universe-base = haskellLib.doJailbreak super.universe-base;
  universe-reverse-instances = haskellLib.doJailbreak super.universe-reverse-instances;
  universe-instances-extended = haskellLib.doJailbreak super.universe-instances-extended;
  stylish-haskell = null; # FIXME

  aeson-gadt-th = haskellLib.doJailbreak super.aeson-gadt-th; # FIXME BROKEN

  snap = haskellLib.doJailbreak super.snap;
  # Exports more internals
  snap-core = haskellLib.dontCheck (self.callCabal2nix "snap-core" (hackGet ../dep/snap-core) {});

  logging-effect = haskellLib.doJailbreak (self.callHackage "logging-effect" "1.3.10" {});
  # unliftio-core = self.callHackage "unliftio-core" "0.2.0.1" {};
  # shelly = self.callHackage "shelly" "1.9.0" {};
  # monad-logger = self.callHackage "monad-logger" "0.3.36" {};
}

{ hackGet, __useNewerCompiler ? false }:

# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) { };
  haskellLib = pkgs.haskell.lib.compose;
  inherit (haskellLib) dontCheck doJailbreak;
in

{
  heist = dontCheck super.heist;
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  hashing = doJailbreak super.hashing;
  lens-family-th = doJailbreak super.lens-family-th;
  repline = doJailbreak super.repline;
  string-interpolate = doJailbreak super.string-interpolate;
  interpolate = dontCheck super.interpolate;
  logging-effect = doJailbreak super.logging-effect;
  hpack = doJailbreak super.hpack;
  nix-derivation = doJailbreak super.nix-derivation;
  brick = doJailbreak super.brick;
  cli-extras = doJailbreak super.cli-extras;
  http-link-header = self.callHackageDirect {
      pkg = "http-link-header";
      ver = "1.2.3";
      sha256 = "sha256-0oopfwTs3lHvt6D5R/ZJ5FJt/OmVDkwRsRtYYAty87E=";
    } {}; # build error is fixed upstream
  cli-nix = doJailbreak super.cli-nix;
  cli-git = doJailbreak super.cli-git;
  nix-thunk = doJailbreak super.nix-thunk;
}

# Everything to build before a release: obelisk's own libraries, and the
# skeleton a project starts from. Each part carries its own matrix and is a
# release of its own.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A lib.checks
#   nix-build release.nix -A lib.tests
#   nix-build release.nix -A skeleton.serverExe.haskell-nix.wasm
#
#   nix-build lib/release.nix -A all
#   nix-build skeleton/release.nix -A all
{ system ? builtins.currentSystem, inputs ? {} }:

let pkgs =
      if inputs ? nixpkgs
      then import inputs.nixpkgs { inherit system; }
      else import ./deps/nix-haskell/pins/nixpkgs { inherit system; };

    lib = pkgs.lib;

in with (import ./nix/libs/prelude { inherit lib inputs; });

let
    # Each part's own `all` is dropped: it names every derivation the part
    # already carries, and would land here a second time under that name.
    part = path: removeAttrs (import path { inherit system inputs; }) [ "all" ];

    released = recurse-for-derivations {
      lib = part ./lib/release.nix;
      skeleton = part ./skeleton/release.nix;
    };

in released // {

  all = pkgs.linkFarm "obelisk-release" (link-farm-entries "" released);

}

# Everything to build before a release: obelisk's own libraries, and the
# skeleton a project starts from. Each part carries its own matrix and is a
# release of its own. `attr-paths` names every derivation here the way
# `nix-build -A` takes it, for a CI matrix that builds each on its own runner.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A lib.checks
#   nix-build release.nix -A lib.tests
#   nix-build release.nix -A skeleton.serverExe.haskell-nix.wasm
#   nix-instantiate --eval --strict --json --read-write-mode release.nix -A attr-paths
#
#   nix-build lib/release.nix -A all
#   nix-build skeleton/release.nix -A all
{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let pkgs = import inputs.nixpkgs { inherit system; };

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

    entries = link-farm-entries "" released;

    # An entry's name walks the tree with slashes, where `nix-build -A`
    # takes dots.
    attr-path = entry: lib.replaceStrings [ "/" ] [ "." ] entry.name;

in released // {

  all = pkgs.linkFarm "obelisk-release" entries;

  # The `-A` path of every derivation above, one per CI runner.
  #
  # Example:
  #
  #   attr-paths
  #   => [ "lib.build.haskell-nix.ghcjs.obelisk-executable-config-inject"
  #        ...
  #        "lib.checks.fourmolu"
  #        ...
  #        "skeleton.shell-build.wasm-meta.haskell-nix.wasm" ]
  attr-paths = map attr-path entries;

}

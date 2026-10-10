# Everything to build before a release: obelisk's own libraries, and the
# skeleton a project starts from. Each part carries its own matrix and is a
# release of its own. `attr-paths` names the cells of those matrices the way
# `nix-build -A` takes them, for a CI matrix that builds each on its own
# runner.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A lib.checks
#   nix-build release.nix -A lib.tests
#   nix-build release.nix -A skeleton.serverExe.haskell-nix.wasm
#   nix-instantiate --eval --strict --json release.nix -A attr-paths
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
    parts = {
      lib = import ./lib/release.nix { inherit system inputs; };
      skeleton = import ./skeleton/release.nix { inherit system inputs; };
    };

    # A part's own `all` and `attr-paths` are dropped from the tree. `all`
    # names every derivation the part already carries, and `attr-paths` is
    # a list of names, which the root joins below under the part's name.
    tree = part: removeAttrs part [ "all" "attr-paths" ];

    released = recurse-for-derivations (lib.mapAttrs (_: tree) parts);

    entries = link-farm-entries "" released;

    # A part's names, under the part's name.
    partEntries = name: part: map (path: "${name}.${path}") part.attr-paths;

in released // {

  all = pkgs.linkFarm "obelisk-release" entries;

  # What a CI matrix builds one job at a time, named the way `nix-build -A`
  # takes it. Each part states its own from its matrix lists alone, so this
  # costs no plan and no build.
  #
  # Example:
  #
  #   attr-paths
  #   => [ "lib.build.haskell-nix.native"
  #        ...
  #        "lib.tests"
  #        "skeleton.containerImage.haskell-nix.js"
  #        ...
  #        "skeleton.shell-build.wasm-meta.haskell-nix.wasm" ]
  attr-paths = lib.concatLists (lib.mapAttrsToList partEntries parts);

}

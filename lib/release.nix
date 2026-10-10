# Every obelisk library, built by each driver for the build platform and for
# each cross target, the style checks over their source, and the test suites
# they carry.
#
#   nix-build lib/release.nix -A all
#   nix-build lib/release.nix -A checks
#   nix-build lib/release.nix -A tests
#   nix-build lib/release.nix -A tests.obelisk-route.roundtrips
#   nix-build lib/release.nix -A build.haskell-nix.wasi32.obelisk-route
#
{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix
}:

let project = import ./default.nix { inherit system inputs; };

    lib = project.pkgs.lib;

in with (import ../nix/libs/prelude { inherit lib inputs; });

let
    # cabal.project also draws reflex-dom in as a source, and those packages
    # belong to that project rather than to this release.
    shipped = lib.attrNames
      (removeAttrs project.nixpkgs.packages
        (lib.attrNames project.config.source-repository-packages));

    # The build platform, then the cross targets. Both drivers reach both: the
    # nixpkgs driver takes its wasm compiler from the ghc-wasm-meta bindist
    # that project.nix imports for it.
    platforms = [ "native" "ghcjs" "wasi32" ];

    # haskell.nix gives a package a tree of components, the nixpkgs driver one
    # derivation. A stanza keeping a package off a cross target leaves a null
    # in place of the whole package, so neither is reached for.
    driverLibrary = {
      haskell-nix = proj: name:
        let pkg = proj.hsPkgs.${name} or null;
        in if pkg == null
           then null
           else pkg.components.library;

      nixpkgs = proj: name: proj.packages.${name} or null;
    };

    drivers = lib.attrNames driverLibrary;

    # The shipped libraries of one project, without the ones a cross target
    # has no build for.
    projectLibraries = driver: proj:
      lib.filterAttrs (_: drv: drv != null)
        (lib.genAttrs shipped (driverLibrary.${driver} proj));

    target = driver: platform:
      if platform == "native"
      then project.${driver}
      else project.${driver}.projectCross.${platform};

    libraries = driver: platform:
      projectLibraries driver (target driver platform);

    # The empty argument takes style.hs's own pin of each tool, so a failure
    # here is the one `nix run github:obsidiansystems/style.hs` reports.
    style = {
      fourmolu = import ../deps/style.hs/fourmolu.nix { inherit system; };
      hlint = import ../deps/style.hs/hlint.nix { inherit system; };
    };

    check = name: tool: command: project.pkgs.runCommand name {
      src = project.config.src-cleaned;
      buildInputs = [ tool ];
    } ''
      set -euo pipefail
      ${command} "$src"
      touch "$out"
    '';

    # Each result set applies its own leaf function to every cell of the one
    # matrix of driver and platform.
    matrix = leaf: recurse-for-derivations
      (lib.genAttrs drivers (driver: lib.genAttrs platforms (leaf driver)));

    released = {

      build = matrix libraries;

      checks = recurse-for-derivations {
        fourmolu = check "check-fourmolu" style.fourmolu "fourmolu --mode check";
        hlint = check "check-hlint" style.hlint "hlint";
      };

      tests = recurse-for-derivations (lib.genAttrs shipped (name:
        removeAttrs project.haskell-nix.hsPkgs.${name}.checks
          [ "recurseForDerivations" ]));

    };

in released // {

  all = project.pkgs.linkFarm "obelisk-lib-release" (link-farm-entries "" released);

}

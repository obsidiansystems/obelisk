# Every obelisk library, built by each driver for the build platform and for
# each cross target, the style checks over their source, and the test suites
# they carry. Each cell is built two ways: as the drivers build it, and as a
# person would inside that driver's shell.
#
#   nix-build lib/release.nix -A all
#   nix-build lib/release.nix -A checks
#   nix-build lib/release.nix -A tests
#   nix-build lib/release.nix -A tests.obelisk-route.roundtrips
#   nix-build lib/release.nix -A build.haskell-nix.wasi32.obelisk-route
#   nix-build lib/release.nix -A shell-build.nixpkgs.ghcjs
{ system ? builtins.currentSystem, inputs ? {} }:

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

    target = driver: platform:
      if platform == "native"
      then project.${driver}
      else project.${driver}.projectCross.${platform};

    libraries = driver: platform:
      lib.filterAttrs (_: drv: drv != null)
        (lib.genAttrs shipped (driverLibrary.${driver} (target driver platform)));

    # The wrapper that puts a cross target's tools ahead of the ones the shell
    # carries for the build platform, named after that target's own prefix.
    dispatcher = driver: platform:
      lib.removeSuffix "-"
        (project.config.${driver}.cross-compiler platform).targetPrefix;

    # cabal needs no package index: the shell's package database already
    # carries every dependency, and naming no repository keeps cabal from
    # fetching one it cannot reach. One `buildCommand` rather than phases,
    # because the nixpkgs driver's shell states its own.
    shellBuild = driver: platform:
      let cross = if platform == "native" then "" else dispatcher driver platform;

          # A cross target's tests are built for a machine this one cannot run.
          test = lib.optionalString (platform == "native")
            "cabal test --offline all";

      in project.${driver}.shell.overrideAttrs (_: {
        name = "obelisk-lib-${driver}-${platform}-shell-build";

        buildCommand = ''
          eval "$shellHook"

          cp -r ${project.config.src-cleaned} source
          chmod -R u+w source
          cd source

          export HOME=$TMPDIR
          export CABAL_CONFIG=$TMPDIR/cabal.config
          echo 'jobs: $ncpus' > $CABAL_CONFIG
          echo 'active-repositories: :none' >> cabal.project.local

          ${cross} cabal build --offline all
          ${test}

          touch $out
        '';
      });

    # The empty argument takes style.hs's own pin of each tool, so a failure
    # here is the one `nix run github:obsidiansystems/style.hs` reports.
    style = {
      fourmolu = import ../deps/style.hs/fourmolu.nix {};
      hlint = import ../deps/style.hs/hlint.nix {};
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

      shell-build = matrix shellBuild;

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

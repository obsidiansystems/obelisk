# Every combination of driver and frontend target the skeleton supports. Each
# is built two ways: as the drivers build it, and as a person would inside the
# project's shell.
#
#   nix-build skeleton/release.nix -A all
#   nix-build skeleton/release.nix -A serverExe
#   nix-build skeleton/release.nix -A serverExe.haskell-nix.wasm
#   nix-build skeleton/release.nix -A shell-build.nixpkgs.wasm
#   nix-instantiate --eval --strict --json skeleton/release.nix -A attr-paths
#
# `wasm-meta` builds the haskell.nix driver's wasm target with a
# ghc-wasm-meta bindist instead of the compiler that driver builds itself.
# The nixpkgs driver takes that bindist from the skeleton, which is the only
# wasm compiler it has.
{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let obelisk = import ../nix { inherit system inputs; };

    variant = modules:
      obelisk.project { imports = [ (import ./project.nix) ] ++ modules; };

    stock = variant [];

    lib = stock.pkgs.lib;

in with (import ../nix/libs/prelude { inherit lib inputs; });

let
    # The bindist follows the driver's own compiler, so a variant never
    # pairs one series with the package set of another.
    wasmMeta = driver: { nix-haskell-compilers, ... }: {
      imports = [
        (import "${nix-haskell-compilers}/ghc-wasm-meta" {
          flavour = lib.versions.majorMinor stock.config.${driver}.compiler-version;
          drivers = [ driver ];
        })
      ];
    };

    haskellNixWasmMeta = variant [ (wasmMeta "haskell-nix") ];

    # The one matrix of driver and wasm-meta. A cell is a project, the driver
    # that builds it, and the path it takes in every result set.
    cells = [

      { path = [ "haskell-nix" ]; project = stock; driver = "haskell-nix"; }

      # The skeleton gives this driver the bindist, which is the only wasm
      # compiler it has.
      { path = [ "nixpkgs" ]; project = stock; driver = "nixpkgs"; }

      # This driver builds a wasm compiler of its own. This cell builds the
      # target with the bindist instead.
      { path = [ "wasm-meta" "haskell-nix" ]; project = haskellNixWasmMeta; driver = "haskell-nix"; }

    ];

    # The tree of one result set: each cell's derivations at the cell's
    # path, every level marked for `nix-build`.
    matrix = set:
      let subtrees = map (cell: lib.setAttrByPath cell.path (set.leaf cell)) cells;
      in recurse-for-derivations (lib.foldl lib.recursiveUpdate {} subtrees);

    # Each result set: what a cell builds, and the names of a cell's entries
    # relative to the set. The names come from the target lists alone, so a
    # CI matrix reads them without evaluating a derivation.
    resultSets = {

      serverExe = {
        leaf = cell: lib.genAttrs targets (target: cell.project.${cell.driver}.serverExe.${target});
        names = cell: targetEntries cell targets;
      };

      containerImage = {
        leaf = cell: lib.genAttrs targets (target: cell.project.${cell.driver}.containerImage.${target});
        names = cell: targetEntries cell targets;
      };

      shell = {
        leaf = cell: cell.project.${cell.driver}.shell;
        names = cell: [ (dotted cell.path) ];
      };

      shell-build = {
        leaf = cell: lib.genAttrs (carried cell.project) (shellBuild cell);
        names = cell: targetEntries cell (carried cell.project);
      };

    };

    # The build a person would run, in the shell they would run it in. cabal
    # needs no package index: the shell's package database already carries
    # everything the project depends on. A configuration naming no
    # repository keeps cabal from fetching one it cannot reach.
    # One `buildCommand` rather than phases: the nixpkgs driver's shell states
    # its own, which stands in for the whole phase list, so a phase written
    # here would never run in that shell.
    shellBuild = cell: target:
      let cross = dispatcher cell.project cell.driver target;

          builddir = frontendTargets.${target}.builddir;

      in cell.project.${cell.driver}.shell.overrideAttrs (old: {
        # The toolchain, not the target: it names the target and tells the
        # bindist cells apart from the ones a driver builds itself.
        name = "obelisk-skeleton-${cell.driver}-${cross}-shell-build";

        nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ nixBuildStandIn ];

        buildCommand = ''
          # What entering the shell runs. It keeps the native and the cross
          # library paths apart, which every native link here needs.
          eval "$shellHook"

          cp -r ${cell.project.config.src-cleaned} source
          chmod -R u+w source
          cd source

          # The custom Setup of obelisk-generated-static-custom runs
          # `static/generate`, which starts with `#!/usr/bin/env bash`. A
          # sandbox carries no /usr/bin/env, where a person's system does.
          patchShebangs .

          export HOME=$TMPDIR
          export CABAL_CONFIG=$TMPDIR/cabal.config
          echo 'jobs: $ncpus' > $CABAL_CONFIG
          echo 'active-repositories: :none' >> cabal.project.local

          # The `if !(arch(javascript) || arch(wasm32))` stanza of
          # cabal.project keeps the backend off the cross target, so the
          # frontend builds for the target and the backend for the build
          # platform. Each keeps its own build directory. One directory for
          # two compilers leaves state that stops the second build.
          ${cross} cabal build --offline exe:frontend --builddir=${builddir}
          cabal build --offline exe:backend

          # cabal knows where it put each executable, and its build
          # directory layout differs by package and target.
          mkdir -p $out
          frontendBin=$(${cross} cabal list-bin exe:frontend --builddir=${builddir})
          backendBin=$(cabal list-bin exe:backend)
          cp -r "$frontendBin" "$backendBin" $out/
        '';
      });

    # The wrapper that puts a cross target's tools ahead of the ones the
    # shell carries for the build platform. It is named after the target's
    # own prefix, so the compiler a target is built with decides it.
    dispatcher = project: driver: target:
      lib.removeSuffix "-"
        (project.config.${driver}.cross-compiler
          frontendTargets.${target}.platform).targetPrefix;

    # A shell carries the tools of the targets `shell.crossPlatforms` names.
    # A build inside it reaches those targets only.
    shellCarries = project: target:
      project.config.${frontendTargets.${target}.flag};

    carried = project: lib.filter (shellCarries project) targets;

    # The name `nix-build -A` takes for a path through the tree.
    dotted = path: lib.concatStringsSep "." path;

    # The names of the chosen targets under a cell.
    targetEntries = cell: chosen:
      map (target: dotted (cell.path ++ [ target ])) chosen;

    # The names of every entry of one result set, under the set's name.
    setEntries = name: set:
      let cellEntries = cell: map (relative: "${name}.${relative}") (set.names cell);
      in lib.concatMap cellEntries cells;

    # A native build runs the custom Setup of obelisk-generated-static-custom,
    # which runs `static/generate`, which builds these assets through nix. A
    # sandbox runs no nix, so the assets come in as an ordinary input and the
    # stand-in below puts them where the script would.
    staticAssets = import ./static { pkgs = stock.pkgs; };

    nixBuildStandIn = stock.pkgs.writeShellScriptBin "nix-build" ''
      output=""
      while [ $# -gt 0 ]; do
        case "$1" in
          -o)
            output="$2"
            shift 2
            ;;
          *)
            shift
            ;;
        esac
      done

      if [ -z "$output" ]; then
        echo "nix-build stand-in: expected -o <path>" >&2
        exit 1
      fi

      ln -sfn ${staticAssets} "$output"
    '';

    released = lib.mapAttrs (_: matrix) resultSets;

    # Each obelisk frontend target: the cross platform it is built for, the
    # option that says whether the project targets it, and the build
    # directory it keeps its own artifacts in.
    frontendTargets = {
      wasm = { platform = "wasi32"; flag = "isWasm"; builddir = "dist-wasm"; };
      js = { platform = "ghcjs"; flag = "isGhcjs"; builddir = "dist-js"; };
    };

    targets = lib.attrNames frontendTargets;

in released // {

  all = stock.pkgs.linkFarm "obelisk-skeleton-release"
    (link-farm-entries "" released);

  # Every derivation here, named the way `nix-build -A` takes it, for a CI
  # matrix that builds each on its own runner.
  #
  # Example:
  #
  #   attr-paths
  #   => [ "containerImage.haskell-nix.js"
  #        ...
  #        "shell.wasm-meta.haskell-nix"
  #        ...
  #        "shell-build.wasm-meta.haskell-nix.wasm" ]
  attr-paths = lib.concatLists (lib.mapAttrsToList setEntries resultSets);

}

# Every combination of driver and frontend target the skeleton is meant to
# work for. Each is built two ways: as the drivers build it, and as a person
# would inside the project's shell.
#
#   nix-build release.nix -A all
#   nix-build release.nix -A serverExe
#   nix-build release.nix -A serverExe.haskell-nix.wasm
#   nix-build release.nix -A shell-build.nixpkgs.wasm
#
# `wasm-meta` builds the haskell.nix driver's wasm target with a
# ghc-wasm-meta bindist instead of the compiler that driver builds itself.
# The nixpkgs driver takes that bindist from the skeleton, which is the only
# wasm compiler it has.
{ system ? builtins.currentSystem, inputs ? {} }:

let obelisk = import ./nix { inherit system inputs; };

    variant = modules:
      obelisk.project { imports = [ (import ./skeleton/project.nix) ] ++ modules; };

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

    stock = variant [];

    haskellNixWasmMeta = variant [ (wasmMeta "haskell-nix") ];

    lib = stock.pkgs.lib;

    # `nix-build` descends into an attribute set only where it is told it
    # may, so every level of a matrix says so.
    walkable = attrs:
      let descend = value:
            if lib.isAttrs value && ! lib.isDerivation value
            then walkable value
            else value;
      in lib.mapAttrs (_: descend) attrs
         // { recurseForDerivations = true; };

    # Every derivation in a tree, named by its path. The path is also where
    # it lands in `all`. The `recurseForDerivations` markers guide
    # `nix-build` and are not entries.
    entries = prefix: value:
      let childName = name:
            if prefix == "" then name else "${prefix}/${name}";
          children = removeAttrs value [ "recurseForDerivations" ];
      in if lib.isDerivation value
         then [ { name = prefix; path = value; } ]
         else lib.concatLists (lib.mapAttrsToList
           (name: entry: entries (childName name) entry)
           children);

    # The one matrix of driver, wasm-meta and frontend target. Each result
    # set applies its own leaf function to every cell.
    matrix = leaf: walkable {

      haskell-nix = leaf stock "haskell-nix" [ "wasm" "js" ];

      # The skeleton gives this driver the bindist, which is the only wasm
      # compiler it has.
      nixpkgs = leaf stock "nixpkgs" [ "wasm" "js" ];

      # This driver builds a wasm compiler of its own. This cell builds the
      # target with the bindist instead.
      wasm-meta.haskell-nix = leaf haskellNixWasmMeta "haskell-nix" [ "wasm" "js" ];

    };

    serverExe = project: driver: targets:
      lib.genAttrs targets (target: project.${driver}.serverExe.${target});

    containerImage = project: driver: targets:
      lib.genAttrs targets (target: project.${driver}.containerImage.${target});

    shell = project: driver: targets: project.${driver}.shell;

    # The build a person would run, in the shell they would run it in. cabal
    # needs no package index: the shell's package database already carries
    # everything the project depends on. A configuration naming no
    # repository keeps cabal from fetching one it cannot reach.
    # One `buildCommand` rather than phases: the nixpkgs driver's shell states
    # its own, which stands in for the whole phase list, so a phase written
    # here would never run in that shell.
    shellBuild = project: driver: targets:
      lib.genAttrs (lib.filter (shellCarries project) targets) (target:
        let cross = dispatcher project driver target;

            builddir = frontendTargets.${target}.builddir;

        in project.${driver}.shell.overrideAttrs (old: {
          # The toolchain, not the target: it names the target and tells the
          # bindist cells apart from the ones a driver builds itself.
          name = "obelisk-skeleton-${driver}-${cross}-shell-build";

          nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ nixBuildStandIn ];

          buildCommand = ''
            # What entering the shell runs. It keeps the native and the cross
            # library paths apart, which every native link here needs.
            eval "$shellHook"

            cp -r ${project.config.src-cleaned} source
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
        }));

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

    # A native build runs the custom Setup of obelisk-generated-static-custom,
    # which runs `static/generate`, which builds these assets through nix. A
    # sandbox runs no nix, so the assets come in as an ordinary input and the
    # stand-in below puts them where the script would.
    staticAssets = import ./skeleton/static { pkgs = stock.pkgs; };

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

    released = {
      serverExe = matrix serverExe;
      containerImage = matrix containerImage;
      shell = matrix shell;
      shell-build = matrix shellBuild;
    };

    # Each obelisk frontend target: the cross platform it is built for, the
    # option that says whether the project targets it, and the build
    # directory it keeps its own artifacts in.
    frontendTargets = {
      wasm = { platform = "wasi32"; flag = "isWasm"; builddir = "dist-wasm"; };
      js = { platform = "ghcjs"; flag = "isGhcjs"; builddir = "dist-js"; };
    };

in released // {

  all = stock.pkgs.linkFarm "obelisk-release" (entries "" released);

}

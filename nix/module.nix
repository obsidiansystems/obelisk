# nix-haskell module that wires obelisk overrides into a project.
# Declares `obelisk.static`, `obelisk.frontend.js`, and `obelisk.frontend.wasm` options;
# `obelisk.frontend.target` selects which pipeline feeds the backend.
{ config, lib, pkgs, system, nix-haskell-patches, ... }:

let obeliskLib = import ./lib.nix { inherit system; };

    perDriver = obeliskLib.perDriver config;

    assets = import ./assets.nix { nixpkgs = pkgs; };

    rawStatic = config.obelisk.static.path;

    # Hash and copy static files into a flat directory with cache-busting names.
    hashedStatic = if rawStatic != null
      then pkgs.runCommand "hashed-static" {
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      } ''
        ${perDriver obeliskLib.obelisk-asset-manifest-generate} --module-only ${rawStatic} "$TMPDIR" Obelisk.Generated.Static $out
      ''
      else null;

    static = config.obelisk.static.compressed;

    frontendJs = config.obelisk.frontend.js.package;

    frontendWasm = config.obelisk.frontend.wasm.package;

    # Select which frontend pipeline feeds the backend based on target.
    frontendOutput =
      if config.obelisk.frontend.target == "wasm"
      then { inherit (config.obelisk.frontend.wasm) optimized compressed; }
      else { inherit (config.obelisk.frontend.js) optimized compressed; };

    compressedFrontendJs = frontendOutput.compressed;

in {
  imports = [
    (import "${nix-haskell-patches}/js/splitmix" { drivers = [ "haskell-nix" ]; })
    (import "${nix-haskell-patches}/wasm/jsaddle-wasm" { })
  ];

  options.obelisk = {
    static = {
      path = lib.mkOption {
        type = lib.types.nullOr (lib.types.either lib.types.path lib.types.package);
        default = null;
        description = "Static assets path or derivation.";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to compress static assets with zopfli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering asset generation.
        #
        # The pipeline must run over the *raw* tree: mkAssets serves each input
        # file under both its original name (an uncacheable redirect) and its
        # hashed name (immutable). The app references hashed names (via
        # Obelisk.Generated.Static, which hashes identically), so feeding the
        # already-hashed tree here would demote every URL the app emits to a
        # no-store redirect, defeating browser caching entirely.
        default = if lib.inNixShell then null
          else if rawStatic == null then null
          else if config.obelisk.static.compress
            then assets.mkAssets rawStatic
            else assets.mkAssetsWith assets.noEncodings rawStatic;
        defaultText = lib.literalExpression "assets.mkAssets rawStatic";
        description = "Static assets preprocessed for obelisk-asset-serve-snap, with optional compression.";
      };
    };

    config.path = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = ''
        Project config directory (with common/, frontend/, backend/ subtrees).
        Its common/ and frontend/ subtrees are bundled into the production
        server as public configs; backend/ is never bundled (it may hold
        secrets; supply those to the running server at runtime).
      '';
    };

    driver = lib.mkOption {
      type = lib.types.enum [ "haskell-nix" "nixpkgs" ];
      default = "haskell-nix";
      description = "The nix-haskell driver the project is built with. The nixpkgs driver only supports the js frontend target.";
    };

    frontend.target = lib.mkOption {
      type = lib.types.enum [ "js" "wasm" ];
      default = perDriver { haskell-nix = "wasm"; nixpkgs = "js"; };
      defaultText = lib.literalExpression ''perDriver { haskell-nix = "wasm"; nixpkgs = "js"; }'';
      description = "Frontend compilation target.";
    };

    frontend.js = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null else perDriver obeliskLib.frontendJs config;
        defaultText = lib.literalExpression "perDriver obeliskLib.frontendJs config";
        description = "GHCJS-compiled frontend derivation.";
      };

      optimization = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to run closure-compiler on frontend JS.";
        };

        level = lib.mkOption {
          type = lib.types.enum [ "BUNDLE" "WHITESPACE_ONLY" "SIMPLE" "TRANSPILE_ONLY" "ADVANCED" ];
          default = "ADVANCED";
          description = "Closure-compiler optimization level.";
        };

        externs = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default = [];
          description = "Extern files passed to closure-compiler via --externs.";
        };

        extraFlags = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = "Extra flags passed to closure-compiler.";
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let opt = config.obelisk.frontend.js.optimization;
              externFlags = map (e: "--externs ${e}") opt.externs;
              flags = lib.concatStringsSep " " ([
                "--language_in" "UNSTABLE"
                "--compilation_level" opt.level
                "--warning_level" "QUIET"
                "--isolation_mode" "IIFE"
                "--assume_function_wrapper"
                "--emit_use_strict"
                "--jscomp_off=undefinedVars"
              ] ++ externFlags ++ opt.extraFlags);
          in if frontendJs == null then null
            else if opt.enable
            then pkgs.runCommand "frontend.jsexe.optimized" {
              nativeBuildInputs = [ pkgs.closurecompiler ];
            } ''
              cp -r ${frontendJs}/bin/frontend.jsexe $out
              chmod -R u+w $out
              closure-compiler ${flags} \
                --externs $out/all.externs.js \
                --js $out/all.js \
                --js_output_file $out/all.js.opt
              mv -f $out/all.js.opt $out/all.js
            ''
            else "${frontendJs}/bin/frontend.jsexe";
        defaultText = lib.literalExpression "closure-compiler frontendJs";
        description = "Closure-compiled frontend jsexe.";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.obelisk.static.compress;
        description = "Whether to compress frontend JS with brotli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let jsexe = config.obelisk.frontend.js.optimized;
          in if jsexe == null then null
            else if config.obelisk.frontend.js.compress
            then assets.mkAssets jsexe
            else assets.mkAssetsWith assets.noEncodings jsexe;
        defaultText = lib.literalExpression "assets.mkAssets optimized";
        description = "Compressed frontend jsexe for obelisk-asset-serve-snap.";
      };
    };

    frontend.wasm = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null else perDriver obeliskLib.frontendWasm config;
        defaultText = lib.literalExpression "perDriver obeliskLib.frontendWasm config";
        description = "WASM-compiled frontend derivation.";
      };

      optimization = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "Whether to run wasm-opt on frontend WASM.";
        };

        level = lib.mkOption {
          type = lib.types.enum [ "0" "1" "2" "3" "4" "s" "z" ];
          default = "2";
          description = "wasm-opt optimization level (-O).";
        };

        extraFlags = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ "-ol" "2" "-s" "1" "--low-memory-unused" "--strip-dwarf" "--converge" ];
          description = "Extra flags passed to wasm-opt.";
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let opt = config.obelisk.frontend.wasm.optimization;
              wasmBin = "${frontendWasm}/bin/frontend.wasm";
              ghc = perDriver {
                haskell-nix = config.haskell-nix.project.projectCross.wasi32.pkg-set.config.ghc.package;
                nixpkgs = config.nixpkgs.project.projectCross.wasi32.haskellPackages.ghc;
              };
              flags = lib.concatStringsSep " " ([ "-all" "-O${opt.level}" ] ++ opt.extraFlags);
          in if frontendWasm == null then null
            else pkgs.runCommand "frontend.jsexe.wasm" {
              nativeBuildInputs = [ pkgs.nodejs pkgs.binaryen pkgs.wasm-tools ];
            } ''
              mkdir -p $out

              # Extract JSFFI bindings
              node $(${ghc}/bin/wasm32-unknown-wasi-ghc --print-libdir)/post-link.mjs \
                -i ${wasmBin} -o $out/ghc_wasm_jsffi.js

              # Optimize and strip WASM binary
              ${if opt.enable
                then ''
                  wasm-opt ${flags} ${wasmBin} -o $out/frontend.wasm
                  wasm-tools strip -a $out/frontend.wasm -o $out/frontend.wasm
                ''
                else ''cp ${wasmBin} $out/frontend.wasm''}

              # Assemble jsexe directory
              cp ${../lib/setup/data/shim.js} $out/all.js
              cp ${obeliskLib.wasi-shim}/dist/*.js $out/
              mv $out/index.js $out/wasi-shim.js
            '';
        defaultText = lib.literalExpression "wasm-opt + post-link.mjs";
        description = "Optimized WASM frontend jsexe directory.";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.obelisk.static.compress;
        description = "Whether to compress frontend WASM with brotli/gzip.";
      };

      compressed = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let jsexe = config.obelisk.frontend.wasm.optimized;
          in if jsexe == null then null
            else if config.obelisk.frontend.wasm.compress
            then assets.mkAssets jsexe
            else assets.mkAssetsWith assets.noEncodings jsexe;
        defaultText = lib.literalExpression "assets.mkAssets optimized";
        description = "Compressed WASM frontend for obelisk-asset-serve-snap.";
      };
    };
  };

  config = {
    _module.args.obeliskLib = obeliskLib;

    inherit (obeliskLib) extraCabalProject;
    # source-repository-packages disabled; using optional-packages in cabal.project instead.
    # inherit (obeliskLib) source-repository-packages;

    compiler-nix-name = lib.mkDefault "ghc914";

    optimizations.all = lib.mkDefault true;

    haskell-nix.overrides = [
      obeliskLib.buildTypeOverride.haskell-nix
      obeliskLib.jsexeOverride.haskell-nix
      (obeliskLib.frontendDataOverride.haskell-nix { static = hashedStatic; compressedStatic = static; })
      (obeliskLib.backendDataOverride.haskell-nix {
        static = hashedStatic;
        compressedStatic = static;
        frontendJs = frontendOutput.optimized;
        inherit compressedFrontendJs;
      })
      (obeliskLib.staticManifestOverride.haskell-nix { static = rawStatic; })
    ];

    nixpkgs.packages = lib.mkMerge [
      obeliskLib.buildTypeOverride.nixpkgs
      (obeliskLib.frontendDataOverride.nixpkgs { static = hashedStatic; compressedStatic = static; })
      (obeliskLib.backendDataOverride.nixpkgs {
        static = hashedStatic;
        compressedStatic = static;
        frontendJs = frontendOutput.optimized;
        inherit compressedFrontendJs;
      })
      (obeliskLib.staticManifestOverride.nixpkgs { static = rawStatic; })
      {
        # without a solver, the arch-conditional flag stanzas of cabal.project
        # cannot be followed; assign the flags for this driver directly
        reflex-dom.flags = {
          use-warp = true;
          webkit2gtk = false;
        };
      }
    ];

    # the nixpkgs driver does not interpret the `packages:` field of
    # cabal.project; the skeleton layout every obelisk project starts from
    nixpkgs.options.packages =
      let packages = config.nixpkgs.options.packages;
      in {
        common.subdir = lib.mkDefault "common";
        frontend.subdir = lib.mkDefault "frontend";
        backend.subdir = lib.mkDefault "backend";
        frontend-js.subdir = lib.mkDefault "${packages.frontend.subdir}/js";
        frontend-wasm.subdir = lib.mkDefault "${packages.frontend.subdir}/wasm";
        obelisk-generated-static.subdir = lib.mkDefault "static/generated";
        obelisk-generated-static-custom.subdir = lib.mkDefault "${packages.obelisk-generated-static.subdir}/custom";
      };

    shell.nativeBuildInputs = [
      (pkgs.writeShellApplication {
        name = "ob-run";
        runtimeInputs = [ pkgs.inotify-tools ];
        text = builtins.readFile ../scripts/ob-run;
      })
      (pkgs.writeShellApplication {
        name = "ob-repl";
        text = builtins.readFile ../scripts/ob-repl;
      })
      (pkgs.writeShellApplication {
        name = "ob-watch";
        runtimeInputs = [ pkgs.haskellPackages.ghcid ];
        text = builtins.readFile ../scripts/ob-watch;
      })
      (pkgs.writeShellApplication {
        name = "ob-hoogle";
        text = builtins.readFile ../scripts/ob-hoogle;
      })
      (pkgs.writeShellApplication {
        name = "ob-deploy";
        runtimeInputs = [ pkgs.openssh ];
        text = builtins.readFile ../scripts/ob-deploy;
      })
      (pkgs.writeShellApplication {
        name = "ob-init";
        runtimeInputs = [ pkgs.git ];
        text = builtins.readFile ../scripts/ob-init;
      })
    ];

    shell.shellHook = ''
      export OBELISK_WASI_SHIM="${obeliskLib.wasi-shim}"
      export OBELISK_SKELETON="${obeliskLib.skeleton}"
      # Where `ob-init --link` points deps/obelisk: this working tree, so a
      # linked scaffold tracks it. The skeleton above is a store copy of
      # skeleton/ alone, so ob-init cannot derive obelisk from its parent.
      export OBELISK_SRC="${toString obeliskLib.src}"

      echo ""
      echo "=== ob-run ==="
      echo ""
      ob-run --help
      echo ""
      echo "=== ob-repl ==="
      echo ""
      ob-repl --help
      echo ""
      echo "=== ob-watch ==="
      echo ""
      ob-watch --help
      echo ""
      echo "=== ob-hoogle ==="
      echo ""
      ob-hoogle --help
      echo ""
      echo "=== ob-deploy ==="
      echo ""
      ob-deploy --help
      echo ""
      echo "=== ob-init ==="
      echo ""
      ob-init --help
      echo ""

      export HOOGLE_PIDFILE="''${TMPDIR:-/tmp}/ob-hoogle.pid"
      HOOGLE_REFSFILE="''${TMPDIR:-/tmp}/ob-hoogle.refs"
      echo $$ >> "$HOOGLE_REFSFILE"
      trap '
        sed -i "/^'$$'$/d" "$HOOGLE_REFSFILE"
        if [ ! -s "$HOOGLE_REFSFILE" ] && [ -f "$HOOGLE_PIDFILE" ]; then
          kill "$(cat "$HOOGLE_PIDFILE")" 2>/dev/null
          rm -f "$HOOGLE_PIDFILE" "$HOOGLE_REFSFILE"
        fi
      ' EXIT
    '';
  };
}

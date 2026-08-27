# nix-haskell module that wires obelisk overrides into a project.
# Declares `obelisk.static`, `obelisk.frontend.js`, and `obelisk.frontend.wasm` options;
# `obelisk.frontend.target` selects which pipeline feeds the backend.
{ config, options, lib, pkgs, system, nix-haskell-patches, ... }:

let obeliskLib = import ./lib.nix { inherit system; };

    perDriver = obeliskLib.perDriver config;

    # The active driver's config namespace. Both drivers declare
    # `cross-exe` and `cross-compiler`.
    driverConfig = config.${config.obelisk.driver};

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

    jsOptimization = config.obelisk.frontend.js.optimization;

    wasmOptimization = config.obelisk.frontend.wasm.optimization;

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
        example = lib.literalExpression "import ./static { inherit pkgs; }";
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to compress static assets with zopfli/gzip.";
        example = false;
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
        example = lib.literalExpression ''pkgs.runCommand "static" {} "cp -r ''${./static-prebuilt} $out"'';
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
      example = lib.literalExpression "./config";
    };

    driver = lib.mkOption {
      type = lib.types.enum [ "haskell-nix" "nixpkgs" ];
      default = "haskell-nix";
      description = ''
        The nix-haskell driver the project is built with. The nixpkgs driver
        has no wasm compiler of its own. It reaches the wasm target only
        through a ghc-wasm-meta bindist.
      '';
      example = "nixpkgs";
    };

    frontend.target = lib.mkOption {
      type = lib.types.enum [ "js" "wasm" ];
      default = perDriver { haskell-nix = "wasm"; nixpkgs = "js"; };
      defaultText = lib.literalExpression ''perDriver { haskell-nix = "wasm"; nixpkgs = "js"; }'';
      description = "Frontend compilation target.";
      example = "js";
    };

    frontend.js = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null
          else driverConfig.cross-exe { platform = "ghcjs"; package = "frontend"; exe = "frontend"; };
        defaultText = lib.literalExpression ''cross-exe { platform = "ghcjs"; package = "frontend"; exe = "frontend"; }'';
        description = "GHCJS-compiled frontend derivation.";
        example = lib.literalExpression ''config.haskell-nix.cross-exe { platform = "ghcjs"; package = "admin"; exe = "admin"; }'';
      };

      optimization = {
        enable = lib.mkOption {
          inherit (options.closure-compiler.enable) type default;
          description = "Whether to run closure-compiler on frontend JS.";
          example = false;
        };

        level = lib.mkOption {
          inherit (options.closure-compiler.level) type default;
          description = "Closure-compiler optimization level.";
          example = "SIMPLE";
        };

        externs = lib.mkOption {
          inherit (options.closure-compiler.externs) type default;
          description = "Files passed as --externs. The jsexe's own all.externs.js goes first.";
          example = lib.literalExpression "[ ./externs.js ]";
        };

        extraFlags = lib.mkOption {
          inherit (options.closure-compiler.extraFlags) type;
          default = [];
          description = "Flags added after the flags closure-compiler declares.";
          example = [ "--formatting PRETTY_PRINT" ];
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          if frontendJs == null then null
          else config.js-optimize {
            platform = "ghcjs";
            package = "frontend";
            exe = "frontend";
            jsexe = "${frontendJs}/bin/frontend.jsexe";
          };
        defaultText = lib.literalExpression ''js-optimize { jsexe = "''${package}/bin/frontend.jsexe"; }'';
        description = "Closure-compiled frontend jsexe.";
        example = lib.literalExpression ''config.js-optimize { platform = "ghcjs"; package = "admin"; exe = "admin"; jsexe = "''${adminJs}/bin/admin.jsexe"; }'';
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.obelisk.static.compress;
        description = "Whether to compress frontend JS with brotli/gzip.";
        example = false;
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
        example = lib.literalExpression "assets.mkAssetsWith assets.noEncodings config.obelisk.frontend.js.optimized";
      };
    };

    frontend.wasm = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        # Skip in nix-shell to avoid triggering cross-compilation builds.
        default = if lib.inNixShell then null
          else driverConfig.cross-exe { platform = "wasi32"; package = "frontend"; exe = "frontend"; };
        defaultText = lib.literalExpression ''cross-exe { platform = "wasi32"; package = "frontend"; exe = "frontend"; }'';
        description = "WASM-compiled frontend derivation.";
        example = lib.literalExpression ''config.haskell-nix.cross-exe { platform = "wasi32"; package = "admin"; exe = "admin"; }'';
      };

      optimization = {
        enable = lib.mkOption {
          inherit (options.wasm-opt.enable) type default;
          description = "Whether to run wasm-opt on frontend WASM.";
          example = false;
        };

        level = lib.mkOption {
          inherit (options.wasm-opt.level) type default;
          description = "wasm-opt optimization level (-O).";
          example = "z";
        };

        extraFlags = lib.mkOption {
          inherit (options.wasm-opt.extraFlags) type default;
          description = "Flags passed to wasm-opt after the level. They replace the declared flags.";
          example = [ "--converge" ];
        };
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          let wasmBin = "${frontendWasm}/bin/frontend.wasm";

              # wasm-optimize copies the binary when the optimizer is off.
              optimizedWasm = config.wasm-optimize {
                platform = "wasi32";
                package = "frontend";
                exe = "frontend";
                wasm = wasmBin;
              };

              # The bindings come from the linked binary, not the optimized one.
              jsffi = config.wasm-jsffi {
                ghc = driverConfig.cross-compiler "wasi32";
                wasm = wasmBin;
              };

          in if frontendWasm == null then null
            else pkgs.runCommand "frontend.jsexe.wasm" {} ''
              mkdir -p $out
              cp ${optimizedWasm} $out/frontend.wasm
              cp ${jsffi} $out/ghc_wasm_jsffi.js
              cp ${../lib/setup/data/shim.js} $out/all.js
              cp ${obeliskLib.wasi-shim}/dist/*.js $out/
              mv $out/index.js $out/wasi-shim.js
            '';
        defaultText = lib.literalExpression "wasm-optimize and wasm-jsffi, with the wasi shim";
        description = "Optimized WASM frontend jsexe directory.";
        example = lib.literalExpression ''pkgs.runCommand "frontend.jsexe.wasm" {} "cp -r ''${./dist-wasm} $out"'';
      };

      compress = lib.mkOption {
        type = lib.types.bool;
        default = config.obelisk.static.compress;
        description = "Whether to compress frontend WASM with brotli/gzip.";
        example = false;
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
        example = lib.literalExpression "assets.mkAssetsWith assets.noEncodings config.obelisk.frontend.wasm.optimized";
      };
    };
  };

  config = {
    _module.args.obeliskLib = obeliskLib;

    inherit (obeliskLib) extraCabalProject;
    # source-repository-packages disabled; using optional-packages in cabal.project instead.
    # inherit (obeliskLib) source-repository-packages;

    optimizations.all = lib.mkDefault true;

    # The obelisk options give the bundle optimizers their top layer. A
    # project can still set any field directly, at any layer.
    closure-compiler = {
      enable = lib.mkDefault jsOptimization.enable;
      level = lib.mkDefault jsOptimization.level;
      externs = lib.mkDefault jsOptimization.externs;

      # This value replaces the declared flags, and the obelisk option adds
      # to them. The declaration therefore goes first.
      extraFlags = lib.mkDefault
        (options.closure-compiler.extraFlags.default ++ jsOptimization.extraFlags);
    };

    wasm-opt = {
      enable = lib.mkDefault wasmOptimization.enable;
      level = lib.mkDefault wasmOptimization.level;
      extraFlags = lib.mkDefault wasmOptimization.extraFlags;
    };

    # The name puts a bundle on the tree. For a javascript target, the
    # haskell.nix driver also installs frontend.jsexe.
    packages.frontend.components.exes.frontend = {};

    haskell-nix.overrides = [
      obeliskLib.buildTypeOverride.haskell-nix
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

    # The `if !(arch(javascript) || arch(wasm32))` stanza of cabal.project,
    # restated for the driver that cannot read it. The warp backend brings
    # in basement, whose cbits recognize no wasi target and stop the build.
    # A platform states the flag over the driver-wide value above.
    nixpkgs.platforms.wasi32.packages.reflex-dom.flags.use-warp = lib.mkDefault false;

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

# nix-haskell module that wires obelisk overrides and hackage overlays
# into a project. Declares `obelisk.static` and `obelisk.frontend.js` options;
# when set, generates hackage overlay and wires assets into frontend/backend data dirs.
# frontend.js defaults to the project's GHCJS-cross-compiled frontend.
{ config, lib, pkgs, system, nix-haskell-patches, ... }:

let obeliskLib = import ./lib.nix { inherit system; };

    assets = import ./assets.nix { nixpkgs = pkgs; };

    rawStatic = config.obelisk.static.path;

    # Hash and copy static files into a flat directory with cache-busting names.
    hashedStatic = if rawStatic != null
      then pkgs.runCommand "hashed-static" {
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      } ''
        ${obeliskLib.obelisk-asset-manifest-generate} --module-only ${rawStatic} "$TMPDIR" Obelisk.Generated.Static $out
      ''
      else null;

    static = config.obelisk.static.compressed;

    frontendJs = config.obelisk.frontend.js.package;

    compressedFrontendJs = config.obelisk.frontend.js.compressed;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
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
        default =
          if hashedStatic != null && config.obelisk.static.compress
            then assets.mkAssets hashedStatic
            else hashedStatic;
        defaultText = lib.literalExpression "assets.mkAssets hashedStatic";
        description = "Hashed static assets after optional compression. Used by overrides.";
      };
    };

    frontend.js = {
      package = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default = obeliskLib.frontendJs config;
        defaultText = lib.literalExpression "obeliskLib.frontendJs config";
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
            else jsexe;
        defaultText = lib.literalExpression "assets.mkAssets optimized";
        description = "Compressed frontend jsexe for obelisk-asset-serve-snap.";
      };
    };
  };

  config = {
    inherit (obeliskLib) extraCabalProject;
    # source-repository-packages disabled; using optional-packages in cabal.project instead.
    # inherit (obeliskLib) source-repository-packages;

    overrides = [
      obeliskLib.buildTypeOverride
      obeliskLib.jsexeOverride
      (obeliskLib.frontendDataOverride { static = hashedStatic; compressedStatic = static; })
      (obeliskLib.backendDataOverride { static = hashedStatic; compressedStatic = static; inherit frontendJs compressedFrontendJs; })
      (obeliskLib.staticManifestOverride { static = rawStatic; })
    ];
  };
}

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

      optimize = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to run closure-compiler on frontend JS.";
      };

      optimized = lib.mkOption {
        type = lib.types.nullOr lib.types.package;
        default =
          if frontendJs == null then null
          else if config.obelisk.frontend.js.optimize
            then pkgs.runCommand "frontend.jsexe.optimized" {
              nativeBuildInputs = [ pkgs.closurecompiler ];
            } ''
              cp -r ${frontendJs}/bin/frontend.jsexe $out
              chmod -R u+w $out
              for js in $out/*.js; do
                closure-compiler --compilation_level SIMPLE "$js" --js_output_file "$js.opt"
                mv -f "$js.opt" "$js"
              done
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

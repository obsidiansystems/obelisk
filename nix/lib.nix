{ system }:

let src = ../.;

    nix-haskell = import ../deps/nix-haskell { inherit system; };

    # Standalone project built only to produce the manifest generator executable.
    obelisk-asset-manifest = nix-haskell {
      name = "obelisk-asset-manifest";
      src = src + "/lib/asset/manifest";
    };

    obelisk-asset-manifest-generate =
      "${obelisk-asset-manifest.haskell-nix.project.hsPkgs.obelisk-asset-manifest.components.exes.obelisk-asset-manifest-generate}/bin/obelisk-asset-manifest-generate";

    # Vendored browser WASI shim for WASM frontend.
    wasi-shim = builtins.fetchTarball {
      url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.3.0.tgz";
      sha256 = "0j8xls87rl2gjr12z4k6jsmc65idrbpilcg9277mlhcrg1l9qdsz";
    };

    # Keep only overrides whose package name exists in the project,
    # so overrides for absent packages are silently skipped.
    mkOptionalPackages = { config, lib }:
      lib.filterAttrs (name: _: config.packages ? ${name});

in {
  inherit src obelisk-asset-manifest-generate wasi-shim;

  frontendJs = config:
    config.haskell-nix.project.projectCross.ghcjs.hsPkgs.frontend.components.exes.frontend;

  frontendWasm = config:
    config.haskell-nix.project.projectCross.wasi32.hsPkgs.frontend.components.exes.frontend;

  source-repository-packages = {
    obelisk-asset-manifest = src + "/lib/asset/manifest";
    obelisk-asset-serve-snap = src + "/lib/asset/serve-snap";
    obelisk-backend = src + "/lib/backend";
    obelisk-executable-config-inject = src + "/lib/executable-config/inject";
    obelisk-executable-config-lookup = src + "/lib/executable-config/lookup";
    obelisk-frontend = src + "/lib/frontend";
    obelisk-route = src + "/lib/route";
    obelisk-snap-extras = src + "/lib/snap-extras";
    obelisk-setup = src + "/lib/setup";
    tabulation = src + "/lib/tabulation";

    reflex-dom = src + "/deps/reflex-dom/reflex-dom";
    reflex-dom-core = src + "/deps/reflex-dom/reflex-dom-core";
  };

  extraCabalProject = [
    (builtins.readFile (src + "/lib/cabal.project.config"))
  ];

  inherit mkOptionalPackages;

  # Generate Obelisk.Generated.Static module before building obelisk-generated-static.
  # Needed because build-type is overridden to Simple (no Setup.hs runs in nix).
  staticManifestOverride = { static }: { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages =
        let preBuild = lib.optionalString (static != null) ''
          rm -rf src
          mkdir -p src
          ${obelisk-asset-manifest-generate} --module-only ${static} . Obelisk.Generated.Static data/static
        '';
        in optional {
          obelisk-generated-static.components.library.preBuild = preBuild;
          obelisk-generated-static-custom.components.library.preBuild = preBuild;
        };
    };

  # Force Simple build type so haskell.nix doesn't run a Setup.hs configure step.
  buildTypeOverride = { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages = optional {
        backend.package.buildType = lib.mkOverride 75 "Simple";
        frontend.package.buildType = lib.mkOverride 75 "Simple";
        frontend-js.package.buildType = lib.mkOverride 75 "Simple";
        frontend-wasm.package.buildType = lib.mkOverride 75 "Simple";
        obelisk-generated-static.package.buildType = lib.mkOverride 75 "Simple";
        obelisk-generated-static-custom.package.buildType = lib.mkOverride 75 "Simple";
      };
    };

  # Copy frontend.jsexe directory into $out/bin after GHCJS build.
  jsexeOverride = { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages = optional {
        frontend.components.exes.frontend.postInstall = ''
          if [ -d dist/build/frontend/frontend.jsexe ]; then
            cp -r dist/build/frontend/frontend.jsexe $out/bin/
          fi
        '';
      };
    };

  # Symlink static assets into frontend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  frontendDataOverride = { static ? null, compressedStatic ? null }:
    ({ config, lib, pkgs, ... }:
      let dataDir = config.packages.frontend.package.dataDir;
      in {
        packages.frontend.components.library.preBuild = lib.optionalString (dataDir != "") ''
          mkdir -p ${dataDir}
          ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
          ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
        '';
        packages.frontend.components.library.postInstall = ''
          for datadir in $data/share/*/*/frontend-*; do
            ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
            ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
          done
        '';
      }
    );

  # Symlink static assets and frontend jsexe into backend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  backendDataOverride = { static ? null, compressedStatic ? null, frontendJs ? null, compressedFrontendJs ? null }:
    ({ config, lib, pkgs, ... }:
      let dataDir = config.packages.backend.package.dataDir;
      in {
        packages.backend.components.library.preBuild = lib.optionalString (dataDir != "") ''
          mkdir -p ${dataDir}
          ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
          ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
          ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} ${dataDir}/frontend.jsexe'' else ""}
          ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} ${dataDir}/frontend.jsexe.assets'' else ""}
        '';
        packages.backend.components.library.postInstall = ''
          for datadir in $data/share/*/*/backend-*; do
            ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
            ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
            ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} "$datadir/frontend.jsexe"'' else ""}
            ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} "$datadir/frontend.jsexe.assets"'' else ""}
          done
        '';
      }
    );

}

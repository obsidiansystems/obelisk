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

    # Keep only overrides whose package name exists in the project,
    # so overrides for absent packages are silently skipped.
    mkOptionalPackages = { config, lib }:
      lib.filterAttrs (name: _: config.packages ? ${name});

    # Generate the Obelisk.Generated.Static Haskell module from static assets.
    obelisk-generated-static-manifest = static:
      obelisk-asset-manifest.nixpkgs.runCommand "obelisk-generated-static" {
        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${obelisk-asset-manifest.nixpkgs.glibcLocales}/lib/locale/locale-archive";
      } ''
        ${obelisk-asset-manifest-generate} ${static} $out obelisk-generated-static Obelisk.Generated.Static $out/files
        sed -i -e 's/GHC\.Internal\.Types/GHC.Types/g' $out/src/Obelisk/Generated/Static.hs
      '';

in {
  inherit src obelisk-asset-manifest obelisk-asset-manifest-generate;

  # Hackage overlay entry for the generated static-asset manifest package.
  obeliskGeneratedStaticOverlay = static: {
    name = "obelisk-generated-static";
    version = "0";
    src = obelisk-generated-static-manifest static;
  };

  frontendJs = config:
    config.haskell-nix.project.projectCross.ghcjs.hsPkgs.frontend.components.exes.frontend;

  source-repository-packages = {
    obelisk-asset-manifest = src + "/lib/asset/manifest";
    obelisk-asset-serve-snap = src + "/lib/asset/serve-snap";
    obelisk-backend = src + "/lib/backend";
    obelisk-executable-config-inject = src + "/lib/executable-config/inject";
    obelisk-executable-config-lookup = src + "/lib/executable-config/lookup";
    obelisk-frontend = src + "/lib/frontend";
    obelisk-route = src + "/lib/route";
    obelisk-snap-extras = src + "/lib/snap-extras";
    tabulation = src + "/lib/tabulation";

    reflex-dom = src + "/deps/reflex-dom/reflex-dom";
    reflex-dom-core = src + "/deps/reflex-dom/reflex-dom-core";
  };

  extraCabalProject = [
    (builtins.readFile (src + "/lib/cabal.project.config"))
  ];

  inherit mkOptionalPackages;

  # Force Simple build type so haskell.nix doesn't run a Setup.hs configure step.
  buildTypeOverride = { config, lib, ... }:
    let optional = mkOptionalPackages { inherit config lib; };
    in {
      packages = optional {
        backend.package.buildType = lib.mkOverride 75 "Simple";
        frontend.package.buildType = lib.mkOverride 75 "Simple";
        frontend-custom.package.buildType = lib.mkOverride 75 "Simple";
        static-manifest.package.buildType = lib.mkOverride 75 "Simple";
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
  frontendDataOverride = { static ? null }:
    ({ config, lib, pkgs, ... }:
      let dataDir = config.packages.frontend.package.dataDir;
      in {
        packages.frontend.components.library.preBuild = lib.optionalString (dataDir != "") ''
          mkdir -p ${dataDir}
          ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
        '';
        packages.frontend.components.library.postInstall = ''
          for datadir in $data/share/*/*/frontend-*; do
            ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
          done
        '';
      }
    );

  # Symlink static assets and frontend jsexe into backend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  backendDataOverride = { static ? null, frontendJs ? null }:
    ({ config, lib, pkgs, ... }:
      let dataDir = config.packages.backend.package.dataDir;
      in {
        packages.backend.components.library.preBuild = lib.optionalString (dataDir != "") ''
          mkdir -p ${dataDir}
          ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
          ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs}/bin/frontend.jsexe ${dataDir}/frontend.jsexe'' else ""}
        '';
        packages.backend.components.library.postInstall = ''
          for datadir in $data/share/*/*/backend-*; do
            ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
            ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs}/bin/frontend.jsexe "$datadir/frontend.jsexe"'' else ""}
          done
        '';
      }
    );

}

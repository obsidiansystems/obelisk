{ system ? builtins.currentSystem
, inputs ? import ../inputs.nix
, pkgs ? import inputs.nixpkgs { inherit system; }
}:

let src = ../.;

    nix-haskell-src = inputs.nix-haskell;

    reflex-dom-src = inputs.reflex-dom;

    nix-haskell = import nix-haskell-src { inherit system pkgs inputs; };

    # jsaddle-warp carrying the batch-pump stall fix
    # (https://github.com/ghcjs/jsaddle/pull/172) until upstream releases it.
    jsaddle-warp-src = builtins.fetchGit {
      url = "https://github.com/ghcjs/jsaddle.git";
      rev = "2152422657602dc29c89140cebd3d7fae080acf1";
    };
    # Standalone project built only to produce the manifest generator executable.
    obelisk-asset-manifest = nix-haskell {
      name = "obelisk-asset-manifest";
      src = src + "/lib/asset/manifest";
    };

in rec {
  inherit src;

  # Select from a per-driver attrset (`{ haskell-nix = ...; nixpkgs = ...; }`)
  # by the driver of an evaluated project config.
  perDriver = config: cases: cases.${config.obelisk.driver};

  # Resolve a nix-thunk dir whether packed or unpacked, and a whole directory
  # of them at once. User projects can pin their own deps as nix-thunks;
  # `source-repository-packages` resolves thunks on its own, so these are for
  # the places that take a plain source.
  thunkSource = import (nix-haskell-src + "/libs/thunk.nix");
  thunkSources = import (nix-haskell-src + "/libs/thunks.nix");

  # Filter a tree through the .gitignore it carries before copying it into the
  # store.
  cleanSource = import (nix-haskell-src + "/libs/clean-source.nix") { inherit pkgs; };

  # The skeleton alone, filtered through its .gitignore. Interpolating
  # `${src}/skeleton` would copy the whole obelisk checkout into the store to
  # hand ob-init a template: deps/nix-haskell/pins carries nixpkgs and
  # haskell.nix, and no filter keeps the build artifacts out either.
  skeleton = cleanSource {
    src = src + "/skeleton";
    name = "obelisk-skeleton";
  };

  obelisk-asset-manifest-generate = {
    haskell-nix = "${obelisk-asset-manifest.haskell-nix.project.hsPkgs.obelisk-asset-manifest.components.exes.obelisk-asset-manifest-generate}/bin/obelisk-asset-manifest-generate";
    nixpkgs = "${obelisk-asset-manifest.nixpkgs.project.packages.obelisk-asset-manifest}/bin/obelisk-asset-manifest-generate";
  };

  # Vendored browser WASI shim for WASM frontend.
  wasi-shim = builtins.fetchTarball {
    url = "https://registry.npmjs.org/@bjorn3/browser_wasi_shim/-/browser_wasi_shim-0.4.2.tgz";
    sha256 = "0wha3xhra6mb6jsfzw7al5pllgd1iyzkxzl9m9110wj5as9rkwlm";
  };

  # Keep only overrides whose package name exists in the project,
  # so overrides for absent packages are silently skipped.
  mkOptionalPackages = { config, lib }:
    lib.filterAttrs (name: _: config.packages ? ${name});

  assets = import ./assets.nix { nixpkgs = pkgs; };

  docs = import ./docs.nix { inherit system inputs pkgs; };

  serverModule = ./server.nix;

  backendExe = {
    haskell-nix = proj: proj.hsPkgs.backend.components.exes.backend;
    nixpkgs = proj: proj.packages.backend;
  };

  # The native obelisk-frontend-html of a project's obelisk-backend: the
  # portable web bundle's bootstrap-HTML generator.
  frontendHtmlExe = {
    haskell-nix = proj: proj.hsPkgs.obelisk-backend.components.exes.obelisk-frontend-html;
    nixpkgs = proj: proj.haskellPackages.obelisk-backend;
  };

  source-repository-packages = {
    obelisk-asset-manifest = src + "/lib/asset/manifest";
    obelisk-asset-serve-snap = src + "/lib/asset/serve-snap";
    obelisk-backend = src + "/lib/backend";
    obelisk-executable-config-inject = src + "/lib/executable-config/inject";
    obelisk-executable-config-lookup = src + "/lib/executable-config/lookup";
    obelisk-frontend = src + "/lib/frontend";
    obelisk-route = src + "/lib/route";
    jsaddle-warp = jsaddle-warp-src + "/jsaddle-warp";
    obelisk-snap-extras = src + "/lib/snap-extras";
    obelisk-setup = src + "/lib/setup";
    tabulation = src + "/lib/tabulation";

    reflex-dom = reflex-dom-src + "/reflex-dom";
    reflex-dom-core = reflex-dom-src + "/reflex-dom-core";
    chrome-test-utils = reflex-dom-src + "/chrome-test-utils";
  };

  extraCabalProject = [
    (builtins.readFile (src + "/lib/cabal.project.config"))
  ];

  # Generate Obelisk.Generated.Static module before building obelisk-generated-static.
  # Needed because build-type is overridden to Simple (no Setup.hs runs in nix).
  staticManifestOverride = {

    haskell-nix = { static }: { config, lib, ... }:
      let optional = mkOptionalPackages { inherit config lib; };
      in {
        packages =
          let preBuild = lib.optionalString (static != null) ''
            rm -rf src
            mkdir -p src
            ${obelisk-asset-manifest-generate.haskell-nix} --module-only ${static} . Obelisk.Generated.Static data/static
          '';
          in optional {
            obelisk-generated-static.components.library.preBuild = preBuild;
            obelisk-generated-static-custom.components.library.preBuild = preBuild;
          };
      };

    # Values for `nixpkgs.packages`: per-package hooks, absent packages are
    # skipped by the driver.
    nixpkgs = { static }:
      let preBuild = pkgs.lib.optionalString (static != null) ''
            rm -rf src
            mkdir -p src
            ${obelisk-asset-manifest-generate.nixpkgs} --module-only ${static} . Obelisk.Generated.Static data/static
          '';
      in {
        obelisk-generated-static.preBuild = preBuild;
        obelisk-generated-static-custom.preBuild = preBuild;
      };

  };

  # Force Simple build type so no custom Setup.hs runs in nix builds.
  buildTypeOverride = {

    haskell-nix = { config, lib, ... }:
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

    # postPatch runs before the generic builder compiles Setup.hs.
    nixpkgs =
      let unCustom = ''
            sed -i 's/^\([Bb]uild-[Tt]ype:[[:space:]]*\)Custom/\1Simple/' *.cabal
            rm -f Setup.hs Setup.lhs
          '';
      in {
        backend.postPatch = unCustom;
        frontend.postPatch = unCustom;
        frontend-js.postPatch = unCustom;
        frontend-wasm.postPatch = unCustom;
        obelisk-generated-static.postPatch = unCustom;
        obelisk-generated-static-custom.postPatch = unCustom;
      };

  };

  # Symlink static assets into frontend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  frontendDataOverride = {

    haskell-nix = { static ? null, compressedStatic ? null }:
      ({ config, lib, pkgs, ... }:
        let optional = mkOptionalPackages { inherit config lib; };
            dataDir = if config.packages ? frontend
              then config.packages.frontend.package.dataDir
              else "";
        in {
          packages = optional {
            frontend.components.library.preBuild = lib.optionalString (dataDir != "") ''
              mkdir -p ${dataDir}
              ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
              ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
            '';
            frontend.components.library.postInstall = ''
              for datadir in $data/share/*/*/frontend-*; do
                ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
                ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
              done
            '';
          };
        }
      );

    nixpkgs = { static ? null, compressedStatic ? null }: {
      frontend.enableSeparateDataOutput = false;
      frontend.preBuild = ''
        dataDir=$(sed -n 's/^[Dd]ata-[Dd]ir:[[:space:]]*//p' *.cabal | head -n1)
        : "''${dataDir:=.}"
        mkdir -p "$dataDir"
        ${if static != null && static != {} then ''ln -sf ${static} "$dataDir/static"'' else ""}
        ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$dataDir/static.assets"'' else ""}
      '';
      frontend.postInstall = ''
        for dataDir in $out/share/*/frontend-* $out/share/*/*/frontend-*; do
          [ -d "$dataDir" ] || continue
          ${if static != null && static != {} then ''ln -sf ${static} "$dataDir/static"'' else ""}
          ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$dataDir/static.assets"'' else ""}
        done
      '';
    };

  };

  # Symlink static assets and frontend jsexe into backend's dataDir
  # at both build time (preBuild) and in the installed output (postInstall).
  backendDataOverride = {

    haskell-nix = { static ? null, compressedStatic ? null, frontendJs ? null, compressedFrontendJs ? null }:
      ({ config, lib, pkgs, ... }:
        let optional = mkOptionalPackages { inherit config lib; };
            dataDir = if config.packages ? backend
              then config.packages.backend.package.dataDir
              else "";
        in {
          packages = optional {
            backend.components.library.preBuild = lib.optionalString (dataDir != "") ''
              mkdir -p ${dataDir}
              ${if static != null && static != {} then ''ln -sf ${static} ${dataDir}/static'' else ""}
              ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} ${dataDir}/static.assets'' else ""}
              ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} ${dataDir}/frontend.jsexe'' else ""}
              ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} ${dataDir}/frontend.jsexe.assets'' else ""}
            '';
            backend.components.library.postInstall = ''
              for datadir in $data/share/*/*/backend-*; do
                ${if static != null && static != {} then ''ln -sf ${static} "$datadir/static"'' else ""}
                ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$datadir/static.assets"'' else ""}
                ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} "$datadir/frontend.jsexe"'' else ""}
                ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} "$datadir/frontend.jsexe.assets"'' else ""}
              done
            '';
          };
        }
      );

    nixpkgs = { static ? null, compressedStatic ? null, frontendJs ? null, compressedFrontendJs ? null }: {
      backend.enableSeparateDataOutput = false;
      backend.preBuild = ''
        dataDir=$(sed -n 's/^[Dd]ata-[Dd]ir:[[:space:]]*//p' *.cabal | head -n1)
        : "''${dataDir:=.}"
        mkdir -p "$dataDir"
        ${if static != null && static != {} then ''ln -sf ${static} "$dataDir/static"'' else ""}
        ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$dataDir/static.assets"'' else ""}
        ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} "$dataDir/frontend.jsexe"'' else ""}
        ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} "$dataDir/frontend.jsexe.assets"'' else ""}
      '';
      backend.postInstall = ''
        for dataDir in $out/share/*/backend-* $out/share/*/*/backend-*; do
          [ -d "$dataDir" ] || continue
          ${if static != null && static != {} then ''ln -sf ${static} "$dataDir/static"'' else ""}
          ${if compressedStatic != null && compressedStatic != {} then ''ln -sf ${compressedStatic} "$dataDir/static.assets"'' else ""}
          ${if frontendJs != null && frontendJs != {} then ''ln -sf ${frontendJs} "$dataDir/frontend.jsexe"'' else ""}
          ${if compressedFrontendJs != null && compressedFrontendJs != {} then ''ln -sf ${compressedFrontendJs} "$dataDir/frontend.jsexe.assets"'' else ""}
        done
      '';
    };

  };

  # Assemble a flat deployment directory for the given frontend target.
  # Contains the backend binary, compressed static/frontend assets.
  mkServerExe = { proj, target }:
    let targetProj = proj.override { obelisk.frontend.target = target; };
        exe = perDriver targetProj.config backendExe targetProj;
        compressedStatic = targetProj.config.obelisk.static.compressed;
        compressedFrontend = targetProj.config.obelisk.frontend.${target}.compressed;
        configPath = targetProj.config.obelisk.config.path;
    in pkgs.runCommand "server-exe" {} ''
      mkdir $out
      set -eux
      ln -s ${exe}/bin/* $out/
      ${pkgs.lib.optionalString (compressedStatic != null) ''
        ln -s ${compressedStatic} $out/static.assets
      ''}
      ${pkgs.lib.optionalString (compressedFrontend != null) ''
        ln -s ${compressedFrontend} $out/frontend.jsexe.assets
      ''}
      ${pkgs.lib.optionalString (configPath != null) ''
        mkdir -p $out/config
      ''}
      ${pkgs.lib.optionalString (configPath != null && builtins.pathExists (configPath + "/common")) ''
        cp -RL ${configPath + "/common"} $out/config/common
      ''}
      ${pkgs.lib.optionalString (configPath != null && builtins.pathExists (configPath + "/frontend")) ''
        cp -RL ${configPath + "/frontend"} $out/config/frontend
      ''}
    '';

  # Assemble the portable web bundle for the given frontend target: a
  # self-contained directory holding a manifest, the client-bootstrap
  # index.html, the optimized compiler output under www/ghcjs, and the
  # content-hashed static tree (hashed files plus original-name aliases,
  # dereferenced to regular files) under www/static. No symlinks, no
  # `.assets` metadata, no precompressed encodings, nothing that depends on
  # the Nix store: the tree is copyable and servable as the web root of any
  # origin. Only public config (config/common, config/frontend) is baked
  # into the HTML; config/backend is never read.
  mkWebBundle = { proj, target }:
    let targetProj = proj.override { obelisk.frontend.target = target; };
        htmlExe = perDriver targetProj.config frontendHtmlExe targetProj;
        optimized = targetProj.config.obelisk.frontend.${target}.optimized;
        hashed = targetProj.config.obelisk.static.hashed;
        configPath = targetProj.config.obelisk.config.path;
    in pkgs.runCommand "obelisk-web-bundle" {
      passthru = { inherit target; };
    } ''
      set -euo pipefail
      mkdir -p $out/www/ghcjs $out/www/static

      # The bootstrap HTML is generated from the project's public configs
      # alone, in a staging directory carrying only those.
      stage=$(mktemp -d)
      mkdir -p "$stage/config"
      ${pkgs.lib.optionalString (configPath != null && builtins.pathExists (configPath + "/common")) ''
        cp -R ${configPath + "/common"} "$stage/config/common"
      ''}
      ${pkgs.lib.optionalString (configPath != null && builtins.pathExists (configPath + "/frontend")) ''
        cp -R ${configPath + "/frontend"} "$stage/config/frontend"
      ''}
      (cd "$stage" && ${htmlExe}/bin/obelisk-frontend-html ${target}) > $out/www/index.html

      # The optimized compiler output, filenames preserved.
      cp -RL ${optimized}/. $out/www/ghcjs/

      # The hashed static tree, original-name aliases dereferenced into
      # regular files. Empty or absent static input leaves an empty dir.
      ${pkgs.lib.optionalString (hashed != null) ''
        cp -RL ${hashed}/. $out/www/static/
      ''}

      # The payload must stay portable: no symlinks and no asset metadata
      # trees may survive the copies above.
      if links=$(find $out -type l) && [ -n "$links" ]; then
        echo "web bundle: symlinks left in payload:" >&2
        echo "$links" >&2
        exit 1
      fi
      if assets=$(find $out -name '*.assets') && [ -n "$assets" ]; then
        echo "web bundle: .assets metadata in payload:" >&2
        echo "$assets" >&2
        exit 1
      fi

      echo '{"format":"obelisk-web-bundle","version":1,"target":"${target}","webDir":"www"}' > $out/obelisk-web-bundle.json
    '';

  # Build an OCI container image (podman/docker) for the given frontend target.
  mkContainerImage = { proj, target, name ? proj.config.name, tag ? "latest" }:
    let serverExe = mkServerExe { inherit proj target; };
        appDir = pkgs.runCommand "app-dir" {} ''
          mkdir -p $out/app
          ln -s ${serverExe}/* $out/app/
        '';
    in pkgs.dockerTools.buildLayeredImage {
      inherit name tag;
      contents = [ appDir pkgs.cacert pkgs.gnutar pkgs.glibcLocales ];
      config = {
        Cmd = [ "/app/backend" "--port=8000" ];
        ExposedPorts = { "8000/tcp" = {}; };
        WorkingDir = "/app";
        Env = [
          "LANG=en_US.UTF-8"
          "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
        ];
      };
    };

}

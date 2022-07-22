{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "13.2"
, config ? {}
, terms ? { # Accepted terms, conditions, and licenses
    security.acme.acceptTerms = false;
  }
, reflex-platform-func ? import ./dep/reflex-platform
}:
let
  reflex-platform = getReflexPlatform { inherit system; };
  inherit (reflex-platform) hackGet nixpkgs;
  pkgs = nixpkgs;

  inherit (import dep/gitignore.nix { inherit (nixpkgs) lib; }) gitignoreSource;

  getReflexPlatform = { system, enableLibraryProfiling ? profiling }: reflex-platform-func {
    inherit iosSdkVersion config system enableLibraryProfiling;

    nixpkgsOverlays = [
      (import ./nixpkgs-overlays)
    ];

    haskellOverlays = [
      (import ./haskell-overlays/misc-deps.nix { inherit hackGet; })
      pkgs.obeliskExecutableConfig.haskellOverlay
      (import ./haskell-overlays/obelisk.nix)
      (import ./haskell-overlays/tighten-ob-exes.nix)
    ];
  };

  # The haskell environment used to build Obelisk itself, e.g. the 'ob' command
  ghcObelisk = reflex-platform.ghc;

  # Development environments for obelisk packages.
  ghcObeliskEnvs = pkgs.lib.mapAttrs (n: v: reflex-platform.workOn ghcObelisk v) ghcObelisk;

  inherit (import ./lib/asset/assets.nix { inherit nixpkgs; }) mkAssets;

  haskellLib = pkgs.haskell.lib;

in rec {
  inherit reflex-platform;
  inherit (reflex-platform) nixpkgs pinBuildInputs;
  inherit (nixpkgs) lib;
  pathGit = ./.;  # Used in CI by the migration graph hash algorithm to correctly ignore files.
  path = reflex-platform.filterGit ./.;
  obelisk = ghcObelisk;
  obeliskEnvs = pkgs.lib.filterAttrs (k: _: k == "tabulation" || pkgs.lib.strings.hasPrefix "obelisk-" k) ghcObeliskEnvs; #TODO: use thunkSet https://github.com/reflex-frp/reflex-platform/pull/671
  command = ghcObelisk.obelisk-command;
  shell = pinBuildInputs "obelisk-shell" ([command] ++ command.commandRuntimeDeps);

  selftest = pkgs.writeScript "selftest" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail

    PATH="${command}/bin:$PATH"
    cd ${./.}
    "${ghcObelisk.obelisk-selftest}/bin/obelisk-selftest" +RTS -N -RTS "$@"
  '';
  skeleton = pkgs.runCommand "skeleton" {
    dir = builtins.filterSource (path: type: builtins.trace path (baseNameOf path != ".obelisk")) ./skeleton;
  } ''
    ln -s "$dir" "$out"
  '';
  nullIfAbsent = p: if lib.pathExists p then p else null;
  #TODO: Avoid copying files within the nix store.  Right now, obelisk-asset-manifest-generate copies files into a big blob so that the android/ios static assets can be imported from there; instead, we should get everything lined up right before turning it into an APK, so that copies, if necessary, only exist temporarily.
  processAssets = { src, packageName ? "obelisk-generated-static", moduleName ? "Obelisk.Generated.Static", exe ? "obelisk-asset-th-generate" }: pkgs.runCommand "asset-manifest" {
    inherit src;
    outputs = [ "out" "haskellManifest" "symlinked" ];
    nativeBuildInputs = [ ghcObelisk.obelisk-asset-manifest ];
  } ''
    set -euo pipefail
    touch "$out"
    mkdir -p "$symlinked"
    ${exe} "$src" "$haskellManifest" ${packageName} ${moduleName} "$symlinked"
  '';

  compressedJs = frontend: optimizationLevel: externs: pkgs.runCommand "compressedJs" {} ''
    set -euo pipefail
    cd '${haskellLib.justStaticExecutables frontend}'
    shopt -s globstar
    for f in **/all.js; do
      dir="$out/$(basename "$(dirname "$f")")"
      mkdir -p "$dir"
      ln -s "$(realpath "$f")" "$dir/all.unminified.js"
      ${if optimizationLevel == null then ''
        ln -s "$dir/all.unminified.js" "$dir/all.js"
      '' else ''
        '${pkgs.closurecompiler}/bin/closure-compiler' ${if externs == null then "" else "--externs '${externs}'"} --externs '${reflex-platform.ghcjsExternsJs}' -O '${optimizationLevel}' --jscomp_warning=checkVars --create_source_map="$dir/all.js.map" --source_map_format=V3 --js_output_file="$dir/all.js" "$dir/all.unminified.js"
        echo '//# sourceMappingURL=all.js.map' >> "$dir/all.js"
      ''}
    done
  '';

  serverModules = {
    mkBaseEc2 = { nixosPkgs, ... }: {...}: {
      imports = [
        (nixosPkgs.path + /nixos/modules/virtualisation/amazon-image.nix)
      ];
      ec2.hvm = true;
    };

    mkDefaultNetworking = { adminEmail, enableHttps, hostName, routeHost, redirectHosts, ... }: {...}: {
      networking = {
        inherit hostName;
        firewall.allowedTCPPorts = if enableHttps then [ 80 443 ] else [ 80 ];
      };

      # `amazon-image.nix` already sets these but if the user provides their own module then
      # forgetting these can cause them to lose access to the server!
      # https://github.com/NixOS/nixpkgs/blob/fab05f17d15e4e125def4fd4e708d205b41d8d74/nixos/modules/virtualisation/amazon-image.nix#L133-L136
      services.openssh.enable = true;
      services.openssh.permitRootLogin = "prohibit-password";

      security.acme = if enableHttps then {
        acceptTerms = terms.security.acme.acceptTerms;
        email = adminEmail;
        certs = {
          "${routeHost}" = {
            extraDomains = builtins.listToAttrs (map (h: { name = h; value = null; }) redirectHosts);
          };
        };
      } else {};
    };

    mkObeliskApp =
      { exe
      , routeHost
      , enableHttps
      , name ? "backend"
      , user ? name
      , group ? user
      , baseUrl ? "/"
      , internalPort ? 8000
      , backendArgs ? "--port=${toString internalPort}"
      , redirectHosts ? [] # Domains to redirect to routeHost; importantly, these domains will be added to the SSL certificate
      , configHash ? "" # The expected hash of the configuration directory tree.
      , ...
      }: {...}:
      assert lib.assertMsg (!(builtins.elem routeHost redirectHosts)) "routeHost may not be a member of redirectHosts";
      {
      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        virtualHosts = {
          "${routeHost}" = {
            enableACME = enableHttps;
            forceSSL = enableHttps;
            locations.${baseUrl} = {
              proxyPass = "http://127.0.0.1:" + toString internalPort;
              proxyWebsockets = true;
              extraConfig = ''
                access_log off;
              '';
            };
          };
        } // builtins.listToAttrs (map (redirectSourceDomain: {
          name = redirectSourceDomain;
          value = {
            enableACME = enableHttps;
            forceSSL = enableHttps;
            globalRedirect = routeHost;
          };
        }) redirectHosts);
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        path = [ pkgs.gnutar ];

        # Even though echoing the hash is functionally useless at
        # runtime, its inclusion in the service script means that Nix
        # will automatically restart the server whenever the configHash
        # argument is changed.
        script = ''
          echo "Expecting config hash to be ${configHash}, but not verifying this"
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} </dev/null
        '';

        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
        };
      };
      users = {
        users.${user} = {
          description = "${user} service";
          home = "/var/lib/${user}";
          createHome = true;
          isSystemUser = true;
          group = group;
        };
        groups.${group} = {};
      };
    };
  };

  inherit mkAssets;

  serverExe = backend: frontend: assets: optimizationLevel: externjs: version:
    pkgs.runCommand "serverExe" {} ''
      mkdir $out
      set -eux
      ln -s '${if profiling then backend else haskellLib.justStaticExecutables backend}'/bin/* $out/
      ln -s '${mkAssets assets}' $out/static.assets
      for d in '${mkAssets (compressedJs frontend optimizationLevel externjs)}'/*/; do
        ln -s "$d" "$out"/"$(basename "$d").assets"
      done
      echo ${version} > $out/version
    '';

  server = { exe, hostName, adminEmail, routeHost, enableHttps, version, module ? serverModules.mkBaseEc2, redirectHosts ? [], configHash ? "" }@args:
    let
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (module { inherit exe hostName adminEmail routeHost enableHttps version; nixosPkgs = pkgs; })
          (serverModules.mkDefaultNetworking args)
          (serverModules.mkObeliskApp args)
        ];
      };
    };

  # An Obelisk project is a reflex-platform project with a predefined layout and role for each component
  project = base': projectDefinition:
    let
      projectOut = { system, enableLibraryProfiling ? profiling }: let reflexPlatformProject = (getReflexPlatform { inherit system enableLibraryProfiling; }).project; in reflexPlatformProject (args@{ nixpkgs, ... }:
        let
          inherit (lib.strings) hasPrefix;
          mkProject =
            { android ? null #TODO: Better error when missing
            , ios ? null #TODO: Better error when missing
            , packages ? {}
            , overrides ? _: _: {}
            , staticFiles ? null
            , tools ? _: []
            , shellToolOverrides ? _: _: {}
            , withHoogle ? false # Setting this to `true` makes shell reloading far slower
            , externjs ? null
            , __closureCompilerOptimizationLevel ? "ADVANCED" # Set this to `null` to skip the closure-compiler step
            , __withGhcide ? false
            , __deprecated ? {}
            }:
            let
              allConfig = nixpkgs.lib.makeExtensible (self: {
                base = base';
                inherit args;
                userSettings = {
                  inherit android ios packages overrides tools shellToolOverrides withHoogle externjs __closureCompilerOptimizationLevel __withGhcide __deprecated;
                  staticFiles = if staticFiles == null then self.base + /static else staticFiles;
                };
                frontendName = "frontend";
                backendName = "backend";
                commonName = "common";
                staticName = "obelisk-generated-static";
                staticFilesImpure = let fs = self.userSettings.staticFiles; in if lib.isDerivation fs then fs else toString fs;
                processedStatic = processAssets {
                  src = self.userSettings.staticFiles;
                  exe = if lib.attrByPath ["userSettings" "__deprecated" "useObeliskAssetManifestGenerate"] false self
                    then builtins.trace "obelisk-asset-manifest-generate is deprecated. Use obelisk-asset-th-generate instead." "obelisk-asset-manifest-generate"
                    else "obelisk-asset-th-generate";
                };
                # The packages whose names and roles are defined by this package
                predefinedPackages = lib.filterAttrs (_: x: x != null) {
                  ${self.frontendName} = nullIfAbsent (self.base + "/frontend");
                  ${self.commonName} = nullIfAbsent (self.base + "/common");
                  ${self.backendName} = nullIfAbsent (self.base + "/backend");
                };
                shellPackages = {};
                combinedPackages = self.predefinedPackages // self.userSettings.packages // self.shellPackages;
                projectOverrides = self': super': {
                  ${self.staticName} = haskellLib.dontHaddock (self'.callCabal2nix self.staticName self.processedStatic.haskellManifest {});
                  ${self.backendName} = haskellLib.addBuildDepend super'.${self.backendName} self'.obelisk-run;
                };
                totalOverrides = lib.composeExtensions self.projectOverrides self.userSettings.overrides;
                privateConfigDirs = ["config/backend"];
                injectableConfig = builtins.filterSource (path: _:
                  !(lib.lists.any (x: hasPrefix (toString self.base + "/" + toString x) (toString path)) self.privateConfigDirs)
                );
                __androidWithConfig = configPath: {
                  ${if self.userSettings.android == null then null else self.frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists self.userSettings.staticFiles then "assets" else null} =
                      nixpkgs.obeliskExecutableConfig.platforms.android.inject
                        (self.injectableConfig configPath)
                        self.processedStatic.symlinked;
                  } // self.userSettings.android;
                };
                __iosWithConfig = configPath: {
                  ${if self.userSettings.ios == null then null else self.frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists self.userSettings.staticFiles then "staticSrc" else null} =
                      nixpkgs.obeliskExecutableConfig.platforms.ios.inject
                        (self.injectableConfig configPath)
                        self.processedStatic.symlinked;
                  } // self.userSettings.ios;
                };

                shells-ghc = builtins.attrNames (self.predefinedPackages // self.shellPackages);

                shells-ghcjs = [
                  self.frontendName
                  self.commonName
                ];

                shells-ghcSavedSplices = [
                  self.commonName
                  self.frontendName
                ];

                shellToolOverrides = lib.composeExtensions
                  self.userSettings.shellToolOverrides
                  (if self.userSettings.__withGhcide
                    then (import ./haskell-overlays/ghcide.nix)
                    else (_: _: {})
                  );

                project = reflexPlatformProject ({...}: self.projectConfig);
                projectConfig = {
                  inherit (self) shellToolOverrides;
                  inherit (self.userSettings) tools withHoogle;
                  overrides = self.totalOverrides;
                  packages = self.combinedPackages;
                  shells = {
                    ${if self.userSettings.android == null && self.userSettings.ios == null then null else "ghcSavedSplices"} =
                      lib.filter (x: lib.hasAttr x self.combinedPackages) self.shells-ghcSavedSplices;
                    ghc = lib.filter (x: lib.hasAttr x self.combinedPackages) self.shells-ghc;
                    ghcjs = lib.filter (x: lib.hasAttr x self.combinedPackages) self.shells-ghcjs;
                  };
                  android = self.__androidWithConfig (self.base + "/config");
                  ios = self.__iosWithConfig (self.base + "/config");

                  passthru = {
                    __unstable__.self = allConfig;
                    inherit (self)
                      staticFilesImpure processedStatic
                      __iosWithConfig __androidWithConfig
                      ;
                    inherit (self.userSettings)
                      android ios overrides packages shellToolOverrides staticFiles tools withHoogle externjs
                      __closureCompilerOptimizationLevel
                      ;
                  };
                };
              });
            in allConfig;
        in (mkProject (projectDefinition args)).projectConfig);
      mainProjectOut = projectOut { inherit system; };
      serverOn = projectInst: version: serverExe
        projectInst.ghc.backend
        mainProjectOut.ghcjs.frontend
        projectInst.passthru.staticFiles
        projectInst.passthru.__closureCompilerOptimizationLevel
        projectInst.passthru.externjs
        version;
      linuxExe = serverOn (projectOut { system = "x86_64-linux"; });
      dummyVersion = "Version number is only available for deployments";
    in mainProjectOut // {
      __unstable__.profiledObRun = let
        profiled = projectOut { inherit system; enableLibraryProfiling = true; };
        exeSource = builtins.toFile "ob-run.hs" ''
          {-# LANGUAGE NoImplicitPrelude #-}
          {-# LANGUAGE PackageImports #-}
          module Main where

          -- Explicitly import Prelude from base lest there be multiple modules called Prelude
          import "base" Prelude (Maybe(Nothing), IO, (++), read)

          import "base" Control.Exception (finally)
          import "reflex" Reflex.Profiled (writeProfilingData)
          import "base" System.Environment (getArgs)

          import qualified "obelisk-run" Obelisk.Run
          import qualified Frontend
          import qualified Backend

          main :: IO ()
          main = do
            [portStr, assets, profFileName] <- getArgs
            Obelisk.Run.run (read portStr) Nothing Nothing (Obelisk.Run.runServeAsset assets) Backend.backend Frontend.frontend
              `finally` writeProfilingData (profFileName ++ ".rprof")
        '';
      in nixpkgs.runCommand "ob-run" {
        buildInputs = [ (profiled.ghc.ghcWithPackages (p: [p.backend p.frontend])) ];
      } ''
        cp ${exeSource} ob-run.hs
        mkdir -p $out/bin
        ghc -x hs -prof -fno-prof-auto -threaded ob-run.hs -o $out/bin/ob-run
      '';

      linuxExeConfigurable = linuxExe;
      linuxExe = linuxExe dummyVersion;
      exe = serverOn mainProjectOut dummyVersion;
      server = args@{ hostName, adminEmail, routeHost, enableHttps, version, module ? serverModules.mkBaseEc2, redirectHosts ? [], configHash ? "" }:
        server (args // { exe = linuxExe version; });
      obelisk = import (base' + "/.obelisk/impl") {};
    };
  haskellPackageSets = {
    inherit (reflex-platform) ghc ghcjs;
  };
}

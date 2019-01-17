{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "10.2"
, __useLegacyCompilers ? false
}:
let

  getReflexPlatform = getReflexPlatform' __useLegacyCompilers;
  getReflexPlatform' = __useLegacyCompilers: sys: import ./dep/reflex-platform {
    inherit iosSdkVersion __useLegacyCompilers;
    system = sys;
    enableLibraryProfiling = profiling;

    nixpkgsOverlays = [
      (import ./nixpkgs-overlays)
    ];

    haskellOverlays = [
      (import ./haskell-overlays/misc-deps.nix)
      (import ./haskell-overlays/obelisk.nix)
      (import ./haskell-overlays/tighten-ob-exes.nix)
    ];
  };

  reflex-platform = getReflexPlatform' false system;
  inherit (reflex-platform) hackGet nixpkgs;
  pkgs = nixpkgs;

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
  obeliskEnvs = ghcObeliskEnvs;
  command = ghcObelisk.obelisk-command;
  shell = pinBuildInputs "obelisk-shell" ([command] ++ command.commandRuntimeDeps) [];

  selftest = pkgs.writeScript "selftest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    PATH="${command}/bin:$PATH"
    export OBELISK_IMPL="${hackGet ./.}"
    "${ghcObelisk.obelisk-selftest}/bin/obelisk-selftest" +RTS -N -RTS "$@"
  '';
  #TODO: Why can't I build ./skeleton directly as a derivation? `nix-build -E ./.` doesn't work
  skeleton = pkgs.runCommand "skeleton" {
    dir = builtins.filterSource (path: type: builtins.trace path (baseNameOf path != ".obelisk")) ./skeleton;
  } ''
    ln -s "$dir" "$out"
  '';
  nullIfAbsent = p: if lib.pathExists p then p else null;
  #TODO: Avoid copying files within the nix store.  Right now, obelisk-asset-manifest-generate copies files into a big blob so that the android/ios static assets can be imported from there; instead, we should get everything lined up right before turning it into an APK, so that copies, if necessary, only exist temporarily.
  processAssets = { src, packageName ? "obelisk-generated-static", moduleName ? "Obelisk.Generated.Static" }: pkgs.runCommand "asset-manifest" {
    inherit src;
    outputs = [ "out" "haskellManifest" "symlinked" ];
    nativeBuildInputs = [ ghcObelisk.obelisk-asset-manifest ];
  } ''
    set -euo pipefail
    touch "$out"
    mkdir -p "$symlinked"
    obelisk-asset-manifest-generate "$src" "$haskellManifest" ${packageName} ${moduleName} "$symlinked"
  '';

  compressedJs = frontend: optimizationLevel: pkgs.runCommand "compressedJs" { buildInputs = [ pkgs.closurecompiler ]; } ''
    mkdir $out
    cd $out
    ln -s "${haskellLib.justStaticExecutables frontend}/bin/frontend.jsexe/all.js" all.unminified.js
    closure-compiler --externs "${reflex-platform.ghcjsExternsJs}" -O ${optimizationLevel} --jscomp_warning=checkVars --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
    echo "//# sourceMappingURL=all.js.map" >> all.js
  '';

  serverModules = {
    mkBaseEc2 = { hostName, routeHost, enableHttps, adminEmail, ... }: {...}: {
      imports = [
        (pkgs.path + /nixos/modules/virtualisation/amazon-image.nix)
      ];
      networking = {
        inherit hostName;
        firewall.allowedTCPPorts = if enableHttps then [ 80 443 ] else [ 80 ];
      };
      security.acme.certs = if enableHttps then {
        "${routeHost}".email = adminEmail;
      } else {};
      ec2.hvm = true;
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
      , ...
      }: {...}: {
      services.nginx = {
        enable = true;
        virtualHosts."${routeHost}" = {
          enableACME = enableHttps;
          forceSSL = enableHttps;
          locations.${baseUrl} = {
            proxyPass = "http://localhost:" + toString internalPort;
          };
        };
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        script = ''
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} >>backend.out 2>>backend.err </dev/null
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

  serverExe = backend: frontend: assets: optimizationLevel: version:
    pkgs.runCommand "serverExe" {} ''
      mkdir $out
      set -eux
      ln -s "${haskellLib.justStaticExecutables backend}"/bin/* $out/
      ln -s "${mkAssets assets}" $out/static.assets
      ln -s ${mkAssets (compressedJs frontend optimizationLevel)} $out/frontend.jsexe.assets
      echo ${version} > $out/version
    '';

  server = { exe, hostName, adminEmail, routeHost, enableHttps, version }@args:
    let
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [
          (serverModules.mkBaseEc2 args)
          (serverModules.mkObeliskApp args)
        ];
      };
    };


  # An Obelisk project is a reflex-platform project with a predefined layout and role for each component
  project = base: projectDefinition:
    let projectOut = sys: (getReflexPlatform sys).project (args@{ nixpkgs, ... }:
          let mkProject = { android ? null #TODO: Better error when missing
                          , ios ? null #TODO: Better error when missing
                          , packages ? {}
                          , overrides ? _: _: {}
                          , staticFiles ? base + /static
                          , tools ? _: []
                          , shellToolOverrides ? _: _: {}
                          , withHoogle ? false # Setting this to `true` makes shell reloading far slower
                          , __closureCompilerOptimizationLevel ? "ADVANCED"
                          }:
              let frontendName = "frontend";
                  backendName = "backend";
                  commonName = "common";
                  staticName = "obelisk-generated-static";
                  staticFilesImpure = if lib.isDerivation staticFiles then staticFiles else toString staticFiles;
                  processedStatic = processAssets { src = staticFiles; };
                  # The packages whose names and roles are defined by this package
                  predefinedPackages = lib.filterAttrs (_: x: x != null) {
                    ${frontendName} = nullIfAbsent (base + "/frontend");
                    ${commonName} = nullIfAbsent (base + "/common");
                    ${backendName} = nullIfAbsent (base + "/backend");
                  };
                  combinedPackages = predefinedPackages // packages;
                  projectOverrides = self: super: {
                    ${staticName} = haskellLib.dontHaddock (self.callCabal2nix staticName processedStatic.haskellManifest {});
                    ${backendName} = haskellLib.addBuildDepend super.${backendName} self.obelisk-run;
                  };
                  totalOverrides = lib.composeExtensions projectOverrides overrides;
                  inherit (lib.strings) hasPrefix;
                  privateConfigDirs = ["config/backend"];
                  injectableConfig = builtins.filterSource (path: _:
                    !(lib.lists.any (x: hasPrefix (toString base + "/" + toString x) (toString path)) privateConfigDirs)
                  );
                  __android = configPath: {
                    ${if android == null then null else frontendName} = {
                      executableName = "frontend";
                      ${if builtins.pathExists staticFiles then "assets" else null} =
                        nixpkgs.obeliskExecutableConfig.platforms.android.inject (injectableConfig configPath) processedStatic.symlinked;
                    } // android;
                  };
                  __ios = configPath: {
                    ${if ios == null then null else frontendName} = {
                      executableName = "frontend";
                      ${if builtins.pathExists staticFiles then "staticSrc" else null} =
                        nixpkgs.obeliskExecutableConfig.platforms.ios.inject (injectableConfig configPath) processedStatic.symlinked;
                    } // ios;
                  };
              in {
                inherit shellToolOverrides tools withHoogle;
                overrides = totalOverrides;
                packages = combinedPackages;
                shells = {
                  ${if android == null && ios == null then null else "ghcSavedSplices"} = (lib.filter (x: lib.hasAttr x combinedPackages) [
                    commonName
                    frontendName
                  ]);
                  ghc = (lib.filter (x: lib.hasAttr x combinedPackages) [
                    backendName
                    commonName
                    frontendName
                  ]);
                  ghcjs = lib.filter (x: lib.hasAttr x combinedPackages) [
                    frontendName
                    commonName
                  ];
                };
                android = __android null;
                ios = __ios null;
                passthru = { inherit android ios packages overrides tools shellToolOverrides withHoogle staticFiles staticFilesImpure __closureCompilerOptimizationLevel processedStatic __ios __android; };
              };
          in mkProject (projectDefinition args));
      serverOn = sys: version: serverExe
        (projectOut sys).ghc.backend
        (projectOut system).ghcjs.frontend
        (projectOut sys).passthru.staticFiles
        (projectOut sys).passthru.__closureCompilerOptimizationLevel
        version;
      linuxExe = serverOn "x86_64-linux";
      dummyVersion = "Version number is only available for deployments";
    in projectOut system // {
      linuxExeConfigurable = linuxExe;
      linuxExe = linuxExe dummyVersion;
      exe = serverOn system dummyVersion;
      server = args@{ hostName, adminEmail, routeHost, enableHttps, version }:
        server (args // { exe = linuxExe version; });
      obelisk = import (base + "/.obelisk/impl") {};
    };
  haskellPackageSets = {
    inherit (reflex-platform) ghc ghcjs;
  };
}

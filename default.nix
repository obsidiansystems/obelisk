{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "10.2"
, config ? {}
}:
let
  cleanSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
    builtins.match "^\\.ghc\\.environment.*" baseName != null ||
    baseName == "cabal.project.local"
  ));

  commandRuntimeDeps = pkgs: with pkgs; [
    coreutils
    git
    nix-prefetch-git
    openssh
  ];

  getReflexPlatform = sys: import ./dep/reflex-platform {
    inherit iosSdkVersion config;
    system = sys;
    enableLibraryProfiling = profiling;

    nixpkgsOverlays = [
      (self: super: {
        obeliskExecutableConfig = import ./lib/executable-config { nixpkgs = pkgs; filterGitSource = cleanSource; };
      })
    ];

    haskellOverlays = [

      # Fix misc upstream packages
      (self: super: let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in {
        hnix = haskellLib.overrideCabal (self.callHackage "hnix" "0.6.1" { these = self.these_0_8; }) (_: {
          jailbreak = true;
          doCheck = false;
        });
        hnix-store-core = self.callHackage "hnix-store-core" "0.1.0.0" {};

        ghcid = self.callCabal2nix "ghcid" (hackGet ./dep/ghcid) {};
      })

      pkgs.obeliskExecutableConfig.haskellOverlay

      # Add obelisk packages
      (self: super: let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        onLinux = pkg: f: if pkgs.stdenv.isLinux then f pkg else pkg;
      in {
        obelisk-executable-config-inject = pkgs.obeliskExecutableConfig.platforms.web.inject self;

        obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" (cleanSource ./lib/asset/manifest) {};
        obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" (cleanSource ./lib/asset/serve-snap) {};
        obelisk-backend = self.callCabal2nix "obelisk-backend" (cleanSource ./lib/backend) {};
        obelisk-cliapp = self.callCabal2nix "obelisk-cliapp" (cleanSource ./lib/cliapp) {};
        obelisk-command = haskellLib.overrideCabal (self.callCabal2nix "obelisk-command" (cleanSource ./lib/command) {}) {
          librarySystemDepends = [
            pkgs.nix
            (haskellLib.justStaticExecutables self.ghcid)
          ];
        };
        obelisk-frontend = self.callCabal2nix "obelisk-frontend" (cleanSource ./lib/frontend) {};
        obelisk-run = onLinux (self.callCabal2nix "obelisk-run" (cleanSource ./lib/run) {}) (pkg:
          haskellLib.overrideCabal pkg (drv: { librarySystemDepends = [ pkgs.iproute ]; })
        );
        obelisk-route = self.callCabal2nix "obelisk-route" (cleanSource ./lib/route) {};
        obelisk-selftest = self.callCabal2nix "obelisk-selftest" (cleanSource ./lib/selftest) {};
        obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (cleanSource ./lib/snap-extras) {};
        tabulation = self.callCabal2nix "tabulation" (cleanSource ./lib/tabulation) {};
      })

      (self: super: let
        pkgs = self.callPackage ({ pkgs }: pkgs) {};
        haskellLib = pkgs.haskell.lib;
      in {
        # Dynamic linking with split objects dramatically increases startup time (about
        # 0.5 seconds on a decent machine with SSD), so we do `justStaticExecutables`.
        obelisk-command = haskellLib.overrideCabal
          (haskellLib.generateOptparseApplicativeCompletion "ob"
            (haskellLib.justStaticExecutables super.obelisk-command))
          (drv: {
            buildTools = (drv.buildTools or []) ++ [ pkgs.buildPackages.makeWrapper ];
            postFixup = ''
              ${drv.postFixup or ""}
              # Make `ob` reference its runtime dependencies.
              wrapProgram "$out"/bin/ob --prefix PATH : ${pkgs.lib.makeBinPath (commandRuntimeDeps pkgs)}
            '';
          });
        obelisk-selftest = haskellLib.justStaticExecutables super.obelisk-selftest;
      })

    ];
  };

  reflex-platform = getReflexPlatform system;
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
  shell = pinBuildInputs "obelisk-shell" ([command] ++ commandRuntimeDeps pkgs);

  selftest = pkgs.writeScript "selftest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    PATH="${command}/bin:$PATH"
    export OBELISK_IMPL="$(mktemp -d)"
    git clone file://${pathGit} $OBELISK_IMPL
    "${ghcObelisk.obelisk-selftest}/bin/obelisk-selftest" +RTS -N -RTS "$@"
  '';
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

  compressedJs = frontend: optimizationLevel: pkgs.runCommand "compressedJs" {} ''
    mkdir $out
    cd $out
    ln -s "${haskellLib.justStaticExecutables frontend}/bin/frontend.jsexe/all.js" all.unminified.js
    ${if optimizationLevel == null then ''
      ln -s all.unminified.js all.js
    '' else ''
      ${pkgs.closurecompiler}/bin/closure-compiler --externs "${reflex-platform.ghcjsExternsJs}" -O ${optimizationLevel} --jscomp_warning=checkVars --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
      echo "//# sourceMappingURL=all.js.map" >> all.js
    ''}
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
            proxyPass = "http://127.0.0.1:" + toString internalPort;
            proxyWebsockets = true;
          };
        };
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        path = [ pkgs.gnutar ];
        script = ''
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
          ./acme.nix
        ];
        disabledModules = [
          (pkgs.path + /nixos/modules/security/acme.nix)
        ];
        nixpkgs.overlays = [
          (self: super: let
            nixos1909 = import (hackGet ./dep/nixpkgs-19.09) {};
          in {
            inherit (nixos1909) simp_le;
          })
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
                          , __closureCompilerOptimizationLevel ? "ADVANCED" # Set this to `null` to skip the closure-compiler step
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
                  __androidWithConfig = configPath: {
                    ${if android == null then null else frontendName} = {
                      executableName = "frontend";
                      ${if builtins.pathExists staticFiles then "assets" else null} =
                        nixpkgs.obeliskExecutableConfig.platforms.android.inject
                          (injectableConfig configPath)
                          processedStatic.symlinked;
                    } // android;
                  };
                  __iosWithConfig = configPath: {
                    ${if ios == null then null else frontendName} = {
                      executableName = "frontend";
                      ${if builtins.pathExists staticFiles then "staticSrc" else null} =
                        nixpkgs.obeliskExecutableConfig.platforms.ios.inject
                          (injectableConfig configPath)
                          processedStatic.symlinked;
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
                android = __androidWithConfig (base + "/config");
                ios = __iosWithConfig (base + "/config");
                passthru = { inherit android ios packages overrides tools shellToolOverrides withHoogle staticFiles staticFilesImpure __closureCompilerOptimizationLevel processedStatic __iosWithConfig __androidWithConfig; };
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

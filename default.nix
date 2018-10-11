{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "10.2"
}:
let
  getReflexPlatform = sys: import ./dep/reflex-platform { inherit iosSdkVersion; system = sys; };
  reflex-platform = getReflexPlatform system;
  inherit (reflex-platform) hackGet nixpkgs;
  pkgs = nixpkgs;
in with pkgs.haskell.lib; with pkgs.lib;
let
  # TODO: Remove this after updating nixpkgs: https://github.com/NixOS/nixpkgs/issues/37750
  justStaticExecutables' = drv: let
      drv' = justStaticExecutables drv;
    in if pkgs.stdenv.isDarwin
      then removeConfigureFlag drv' "--ghc-option=-optl=-dead_strip"
      else drv';

  commandRuntimeDeps = pkgs: with pkgs; [
    coreutils
    git
    gitAndTools.hub
    nix-prefetch-git
    openssh
  ];

  #TODO: Upstream
  # Modify a Haskell package to add completion scripts for the given
  # executable produced by it.  These completion scripts will be picked up
  # automatically if the resulting derivation is installed, e.g. by
  # `nix-env -i`.
  addOptparseApplicativeCompletionScripts = exeName: pkg: overrideCabal pkg (drv: {
    postInstall = (drv.postInstall or "") + ''
      BASH_COMP_DIR="$out/share/bash-completion/completions"
      mkdir -p "$BASH_COMP_DIR"
      "$out/bin/${exeName}" --bash-completion-script "$out/bin/${exeName}" >"$BASH_COMP_DIR/ob"

      ZSH_COMP_DIR="$out/share/zsh/vendor-completions"
      mkdir -p "$ZSH_COMP_DIR"
      "$out/bin/${exeName}" --zsh-completion-script "$out/bin/${exeName}" >"$ZSH_COMP_DIR/_ob"

      FISH_COMP_DIR="$out/share/fish/vendor_completions.d"
      mkdir -p "$FISH_COMP_DIR"
      "$out/bin/${exeName}" --fish-completion-script "$out/bin/${exeName}" >"$FISH_COMP_DIR/ob.fish"
    '';
  });

  # The haskell environment used to build Obelisk itself, e.g. the 'ob' command
  ghcObelisk = reflex-platform.ghc.override {
    overrides = composeExtensions defaultHaskellOverrides (self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });

      # Dynamic linking with split objects dramatically increases startup time (about 0.5 seconds on a decent machine with SSD)
      obelisk-command = addOptparseApplicativeCompletionScripts "ob" (justStaticExecutables' super.obelisk-command);
    });
  };

  fixUpstreamPkgs = self: super: {
    algebraic-graphs = pkgs.haskell.lib.doJailbreak
      (self.callCabal2nix "algebraic-graphs" (pkgs.fetchFromGitHub {
        owner = "snowleopard";
        repo = "alga";
        rev = "480a73137e9b38ad3f1bc2c628847953d2fb3e25";
        sha256 = "0dpwi5ffs88brl3lz51bwb004c6zm8ds8pkw1vzsg2a6aaiyhlzl";
      }) {});

    # Need deriveSomeUniverse
    # PR: https://github.com/dmwit/universe/pull/32
    universe-template = self.callCabal2nix "universe-template" (pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "universe";
      rev = "5a2fc823caa4163411d7e41aa80e67cefb15944a";
      sha256 = "0ll2z0fh18z6x8jl8kbp7ldagwccz3wjmvrw1gw752z058n82yfa";
    } + /template) {};
  };

  cleanSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
    builtins.match "^\\.ghc\\.environment.*" baseName != null ||
    baseName == "cabal.project.local"
  ));

  executableConfig = import ./lib/executable-config { nixpkgs = pkgs; filterGitSource = cleanSource; };

  addLibs = self: super: {
    obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" (hackGet ./lib/asset + "/manifest") {};
    obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" (hackGet ./lib/asset + "/serve-snap") {};
    obelisk-backend = self.callCabal2nix "obelisk-backend" (cleanSource ./lib/backend) {};
    obelisk-cliapp = self.callCabal2nix "obelisk-cliapp" (cleanSource ./lib/cliapp) {};
    obelisk-command = (self.callCabal2nix "obelisk-command" (cleanSource ./lib/command) {}).overrideAttrs
      (drv: {
        buildInputs = drv.buildInputs ++ [ pkgs.makeWrapper ];
        postInstall = ''
          ${drv.postInstall or ""}
          # Install migrations
          cp -r ${./migration} $out/migration;
        '';
        postFixup = ''
          ${drv.postFixup or ""}
          # Make `ob` reference its runtime dependencies.
          wrapProgram "$out"/bin/ob --prefix PATH : ${pkgs.lib.makeBinPath (commandRuntimeDeps pkgs)}
        '';
      });
    obelisk-executable-config = executableConfig.haskellPackage self;
    obelisk-executable-config-inject = executableConfig.platforms.web.inject self;
    obelisk-frontend = self.callCabal2nix "obelisk-frontend" (cleanSource ./lib/frontend) {};
    obelisk-migration = self.callCabal2nix "obelisk-migration" (cleanSource ./lib/migration) {};
    obelisk-run = self.callCabal2nix "obelisk-run" (cleanSource ./lib/run) {};
    obelisk-route = self.callCabal2nix "obelisk-route" (cleanSource ./lib/route) {};
    obelisk-selftest = self.callCabal2nix "obelisk-selftest" (cleanSource ./lib/selftest) {};
    obelisk-snap = self.callCabal2nix "obelisk-snap" (cleanSource ./lib/snap) {};
    obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (cleanSource ./lib/snap-extras) {};
  };

  inherit (import ./lib/asset/assets.nix { inherit nixpkgs; }) mkAssets;

  defaultHaskellOverrides = composeExtensions fixUpstreamPkgs addLibs;
in
with pkgs.lib;
rec {
  inherit reflex-platform;
  inherit (reflex-platform) nixpkgs pinBuildInputs;
  pathGit = ./.;  # Used in CI by the migration graph hash algorithm to correctly ignore files.
  path = reflex-platform.filterGit ./.;
  obelisk = ghcObelisk;
  command = ghcObelisk.obelisk-command;
  shell = pinBuildInputs "obelisk-shell" ([command] ++ commandRuntimeDeps pkgs) [];

  selftest = pkgs.writeScript "selftest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    PATH="${command}/bin:$PATH"
    export OBELISK_IMPL="${hackGet ./.}"
    "${justStaticExecutables' ghcObelisk.obelisk-selftest}/bin/obelisk-selftest" "$@"
  '';
  #TODO: Why can't I build ./skeleton directly as a derivation? `nix-build -E ./.` doesn't work
  skeleton = pkgs.runCommand "skeleton" {
    dir = builtins.filterSource (path: type: builtins.trace path (baseNameOf path != ".obelisk")) ./skeleton;
  } ''
    ln -s "$dir" "$out"
  '';
  nullIfAbsent = p: if pathExists p then p else null;
  haskellOverrides = addLibs;
  #TODO: Avoid copying files within the nix store.  Right now, obelisk-asset-manifest-generate copies files into a big blob so that the android/ios static assets can be imported from there; instead, we should get everything lined up right before turning it into an APK, so that copies, if necessary, only exist temporarily.
  processAssets = { src, packageName ? "obelisk-generated-static", moduleName ? "Obelisk.Generated.Static" }: pkgs.runCommand "asset-manifest" {
    inherit src;
    outputs = [ "out" "haskellManifest" "symlinked" ];
    buildInputs = [
      (reflex-platform.ghc.callCabal2nix "obelisk-asset-manifest" (hackGet ./lib/asset + "/manifest") {})
    ];
  } ''
    set -euo pipefail
    touch "$out"
    obelisk-asset-manifest-generate "$src" "$haskellManifest" ${packageName} ${moduleName} "$symlinked"
  '';

  compressedJs = frontend: pkgs.runCommand "compressedJs" { buildInputs = [ pkgs.closurecompiler ]; } ''
    mkdir $out
    cd $out
    ln -s "${justStaticExecutables frontend}/bin/frontend.jsexe/all.js" all.unminified.js
    closure-compiler --externs "${reflex-platform.ghcjsExternsJs}" -O ADVANCED --jscomp_warning=checkVars --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
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

  serverExe = backend: frontend: assets: config:
    pkgs.runCommand "serverExe" {} ''
      mkdir $out
      set -eux
      ln -s "${justStaticExecutables backend}"/bin/* $out/
      ln -s "${mkAssets assets}" $out/static.assets
      cp -r ${config} $out/config
      ln -s ${mkAssets (compressedJs frontend)} $out/frontend.jsexe.assets
    '';

  server = { exe, hostName, adminEmail, routeHost, enableHttps, config }@args:
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
    let configPath = base + "/config";
        static = base + "/static";
        processedStatic = processAssets { src = static; };
        projectOut = sys: (getReflexPlatform sys).project (args@{ nixpkgs, ... }:
          let mkProject = { android ? null #TODO: Better error when missing
                          , ios ? null #TODO: Better error when missing
                          , packages ? {}
                          , overrides ? _: _: {}
                          , tools ? _: []
                          , shellToolOverrides ? _: _: {}
                          , withHoogle ? false # Setting this to `true` makes shell reloading far slower
                          }:
              let frontendName = "frontend";
                  backendName = "backend";
                  commonName = "common";
                  staticName = "obelisk-generated-static";
                  staticPath = base + "/static";
                  # The packages whose names and roles are defined by this package
                  predefinedPackages = filterAttrs (_: x: x != null) {
                    ${frontendName} = nullIfAbsent (base + "/frontend");
                    ${commonName} = nullIfAbsent (base + "/common");
                    ${backendName} = nullIfAbsent (base + "/backend");
                  };
                  combinedPackages = predefinedPackages // packages;
                  projectOverrides = self: super: {
                    ${staticName} = dontHaddock (self.callCabal2nix "static" processedStatic.haskellManifest {});
                    ${backendName} = addBuildDepend super.${backendName} self.obelisk-run;
                  };
                  totalOverrides = composeExtensions (composeExtensions defaultHaskellOverrides projectOverrides) overrides;
                  inherit (nixpkgs) lib;
                  inherit (lib.strings) hasPrefix;
                  privateConfigDirs = ["config/backend"];
                  injectableConfig = builtins.filterSource (path: _:
                    !(lib.lists.any (x: hasPrefix (toString base + "/" + toString x) (toString path)) privateConfigDirs)
                  ) configPath;
              in {
                inherit shellToolOverrides tools withHoogle;
                overrides = totalOverrides;
                packages = combinedPackages;
                shells = {
                  ghcSavedSplices = (filter (x: hasAttr x combinedPackages) [
                    commonName
                    frontendName
                  ]);
                  ghc = (filter (x: hasAttr x combinedPackages) [
                    backendName
                    commonName
                    frontendName
                  ]);
                  ghcjs = filter (x: hasAttr x combinedPackages) [
                    frontendName
                    commonName
                  ];
                };
                android = {
                  ${if android == null then null else frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists staticPath then "assets" else null} =
                      executableConfig.platforms.android.inject injectableConfig processedStatic.symlinked;
                  } // android;
                };
                ios = {
                  ${if ios == null then null else frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists staticPath then "staticSrc" else null} =
                      executableConfig.platforms.ios.inject injectableConfig processedStatic.symlinked;
                  } // ios;
                };
              };
          in mkProject (projectDefinition args));
      serverOn = sys: config: serverExe (projectOut sys).ghc.backend (projectOut system).ghcjs.frontend static config;
      # `exe` is project's backend executable, with frontend assets, config, etc.
      # `linuxExe` is the same but built for x86_64-linux.
      exe = serverOn system configPath;
      linuxExe = serverOn "x86_64-linux" configPath;
    in projectOut system // {
      inherit exe linuxExe;
      server = args@{ hostName, adminEmail, routeHost, enableHttps, config }:
        server (args // { exe = serverOn "x86_64-linux" config;});
      obelisk = import (base + "/.obelisk/impl") {};
    };
  haskellPackageSets = {
    ghc = reflex-platform.ghc.override {
      overrides = defaultHaskellOverrides;
    };
  };
}

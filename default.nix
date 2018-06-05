{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "10.2"
}:
let
  getReflexPlatform = sys: import ./dep/reflex-platform { inherit iosSdkVersion; system = sys; };
  reflex-platform = getReflexPlatform system;
  inherit (reflex-platform) hackGet;
  pkgs = reflex-platform.nixpkgs;
in with pkgs.haskell.lib; with pkgs.lib;
let
  # TODO: Remove this after updating nixpkgs: https://github.com/NixOS/nixpkgs/issues/37750
  justStaticExecutables' = drv: let
      drv' = justStaticExecutables drv;
    in if pkgs.stdenv.isDarwin
      then removeConfigureFlag drv' "--ghc-option=-optl=-dead_strip"
      else drv';

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

      #TODO: Eliminate this when https://github.com/phadej/github/pull/307 makes its way to reflex-platform
      github = overrideCabal super.github (drv: {
        src = pkgs.fetchFromGitHub {
          owner = "ryantrinkle";
          repo = "github";
          rev = "8f543cdc07876bfb7b924d3722e3dbc1df4b02ca";
          sha256 = "0vcnx9cxqd821kmjx1r4cvj95zs742qm1pwqnb52vw3djplbqd86";
        };
        sha256 = null;
        revision = null;
        editedCabalFile = null;
      });

      # Dynamic linking with split objects dramatically increases startup time (about 0.5 seconds on a decent machine with SSD)
      obelisk-command = addOptparseApplicativeCompletionScripts "ob" (justStaticExecutables' super.obelisk-command);

      optparse-applicative = self.callHackage "optparse-applicative" "0.14.0.0" {};
    });
  };

  fixUpstreamPkgs = self: super: {
    heist = doJailbreak super.heist; #TODO: Move up to reflex-platform; create tests for r-p supported packages
    modern-uri =
      let src = pkgs.fetchFromGitHub {
            owner = "mrkkrp";
            repo = "modern-uri";
            rev = "21064285deb284cb3328094c69c34f9f67919cc9";
            sha256 = "0vddw8r9sb31h1fz1anzxrs9p3a3p8ygpxlj398z5j47wmr86cmi";
          };
      in (overrideCabal (self.callCabal2nix "modern-uri" src {}) (drv: {
            doCheck = false;
            postPatch = (drv.postPatch or "") + ''
              substituteInPlace Text/URI/Types.hs \
                --replace "instance Arbitrary (NonEmpty (RText 'PathPiece)) where" "" \
                --replace "  arbitrary = (:|) <$> arbitrary <*> arbitrary" ""
            '';
          })).override { megaparsec = super.megaparsec_6_1_1; };
    network-transport = self.callHackage "network-transport" "0.5.2" {};
    network-transport-tcp = self.callHackage "network-transport-tcp" "0.6.0" {};
  };

  cleanSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
    builtins.match "^\\.ghc\\.environment.*" baseName != null ||
    baseName == "cabal.project.local"
  ));

  executableConfig = import ./lib/executable-config { nixpkgs = pkgs; filterGitSource = cleanSource; };

  addLibs = self: super: {
    obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" (hackGet ./lib/asset + "/manifest") {};
    obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" (hackGet ./lib/asset + "/serve-snap") {};
    obelisk-cliapp = self.callCabal2nix "obelisk-cliapp" (cleanSource ./lib/cliapp) {};
    obelisk-backend = self.callCabal2nix "obelisk-backend" (cleanSource ./lib/backend) {};
    obelisk-command = (self.callCabal2nix "obelisk-command" (cleanSource ./lib/command) {}).override { Cabal = super.Cabal_2_0_0_2; };
    obelisk-executable-config = executableConfig.haskellPackage self;
    obelisk-executable-config-inject = executableConfig.platforms.web.inject self; # TODO handle platforms.{ios,android}
    obelisk-run = self.callCabal2nix "obelisk-run" (cleanSource ./lib/run) {};
    obelisk-selftest = self.callCabal2nix "obelisk-selftest" (cleanSource ./lib/selftest) {};
    obelisk-snap = self.callCabal2nix "obelisk-snap" (cleanSource ./lib/snap) {};
    obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (cleanSource ./lib/snap-extras) {};
  };

  defaultHaskellOverrides = composeExtensions fixUpstreamPkgs addLibs;
in
with pkgs.lib;
rec {
  inherit reflex-platform;
  inherit (reflex-platform) nixpkgs pinBuildInputs;
  path = reflex-platform.filterGit ./.;
  obelisk = ghcObelisk;
  command = ghcObelisk.obelisk-command;
  shell = pinBuildInputs "obelisk-shell" ([
    command
    pkgs.openssh
    pkgs.gitAndTools.hub
  ]) [];

  selftest = pkgs.writeScript "selftest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    PATH="${ghcObelisk.obelisk-command}/bin:$PATH"
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
  processAssets = { src, packageName ? "static", moduleName ? "Static" }: pkgs.runCommand "asset-manifest" {
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
      closure-compiler --externs "${reflex-platform.ghcjsExternsJs}" -O ADVANCED --create_source_map="all.js.map" --source_map_format=V3 --js_output_file="all.js" all.unminified.js
      echo "//# sourceMappingURL=all.js.map" >> all.js
  '';

  serverExe = backend: frontend: assets: config:
    pkgs.runCommand "serverExe" {} ''
      mkdir $out
      set -eux
      ln -s "${justStaticExecutables backend}"/bin/backend $out/backend
      ln -s "${assets}" $out/static
      ln -s "${config}" $out/config
      ln -s ${compressedJs frontend} $out/frontend.jsexe
    ''; #TODO: run frontend.jsexe through the asset processing pipeline

  server = exe: hostName: sslHost: sslEmail:
    let system = "x86_64-linux";
        nixos = import (pkgs.path + /nixos);
        https = (import lib/https {}).module {
          backendPort = 8000; # TODO read from config
          sslConfig = {
            hostName = sslHost;
            adminEmail = sslEmail;
            subdomains = [ ];
          };
        };
    in nixos {
      inherit system;
      configuration = args: {
        imports = [
          (pkgs.path + /nixos/modules/virtualisation/amazon-image.nix)
          https
        ];
        networking = { inherit hostName; };
        systemd.services.backend = {
          wantedBy = [ "multi-user.target" ];
          after = [ "network.target" ];
          restartIfChanged = true;
          script = ''
            ln -sft . "${exe}"/*
            mkdir -p log
            exec ./backend >>backend.out 2>>backend.err </dev/null
          '';
          serviceConfig = {
            User = "backend";
            KillMode = "process";
            WorkingDirectory = "~";
            Restart = "always";
            RestartSec = 5;
          };
        };
        users.extraUsers.backend = {
          description = "backend service";
          home = "/var/lib/backend";
          createHome = true;
          isSystemUser = true;
          group = "backend";
        };
        ec2.hvm = true;
      };
    };


  # An Obelisk project is a reflex-platform project with a predefined layout and role for each component
  project = base: projectDefinition:
    let assets = processAssets { src = base + "/static"; };
        configPath = base + "/config";
        projectOut = sys: (getReflexPlatform sys).project (args@{ nixpkgs, ... }:
          let mkProject = { android ? null #TODO: Better error when missing
                          , ios ? null #TODO: Better error when missing
                          , packages ? {}
                          , overrides ? _: _: {}
                          }:
              let frontendName = "frontend";
                  backendName = "backend";
                  commonName = "common";
                  staticName = "static";
                  staticPath = base + "/static";
                  assets = processAssets { src = base + "/static"; };
                  # The packages whose names and roles are defined by this package
                  predefinedPackages = filterAttrs (_: x: x != null) {
                    ${frontendName} = nullIfAbsent (base + "/frontend");
                    ${commonName} = nullIfAbsent (base + "/common");
                    ${backendName} = nullIfAbsent (base + "/backend");
                  };
                  combinedPackages = predefinedPackages // packages;
                  projectOverrides = self: super: {
                    ${staticName} = dontHaddock (self.callCabal2nix "static" assets.haskellManifest {});
                    ${backendName} = addBuildDepend super.${backendName} self.obelisk-run;
                  };
                  totalOverrides = composeExtensions (composeExtensions defaultHaskellOverrides projectOverrides) overrides;
              in {
                overrides = totalOverrides;
                packages = combinedPackages;
                shells = {
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
                withHoogle = false; # Setting this to `true` makes shell reloading far slower
                android = {
                  ${if android == null then null else frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists staticPath then "assets" else null} = assets.symlinked;
                  } // android;
                };
                ios = {
                  ${if ios == null then null else frontendName} = {
                    executableName = "frontend";
                    ${if builtins.pathExists staticPath then "staticSrc" else null} = assets.symlinked;
                  } // ios;
                };
              };
          in mkProject (projectDefinition args));
    in projectOut system // {
      server = { hostName, sslHost, sslEmail }:
        let exe = serverExe (projectOut "x86_64-linux").ghc.backend (projectOut system).ghcjs.frontend assets.symlinked configPath;
        in server exe hostName sslHost sslEmail;
      obelisk = import (base + "/.obelisk/impl") {};
    };
  haskellPackageSets = {
    ghc = reflex-platform.ghc.override {
      overrides = defaultHaskellOverrides;
    };
  };
}

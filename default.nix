{ system ? builtins.currentSystem
, profiling ? false
}:
let reflex-platform = import ./dep/reflex-platform { inherit system; };
    inherit (reflex-platform) hackGet;
    pkgs = reflex-platform.nixpkgs;
in with pkgs.haskell.lib; with pkgs.lib;
let #TODO: Upstream
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
        obelisk-command = addOptparseApplicativeCompletionScripts "ob" (justStaticExecutables super.obelisk-command);

        optparse-applicative = self.callHackage "optparse-applicative" "0.14.0.0" {};
      });
    };

    fixUpstreamPkgs = self: super: {
      heist = doJailbreak super.heist; #TODO: Move up to reflex-platform; create tests for r-p supported packages
      network-transport = self.callHackage "network-transport" "0.5.2" {};
      network-transport-tcp = self.callHackage "network-transport-tcp" "0.6.0" {};
    };

    cleanSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
      builtins.match "^\\.ghc\\.environment.*" baseName != null ||
      baseName == "cabal.project.local"
    ));

    addLibs = self: super: {
      obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" (hackGet ./lib/asset + "/manifest") {};
      obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" (hackGet ./lib/asset + "/serve-snap") {};
      obelisk-backend = self.callCabal2nix "obelisk-backend" (cleanSource ./lib/backend) {};
      obelisk-command = (self.callCabal2nix "obelisk-command" (cleanSource ./lib/command) {}).override { Cabal = super.Cabal_2_0_0_2; };
      obelisk-run-frontend = self.callCabal2nix "obelisk-run-frontend" (cleanSource ./lib/run-frontend) {};
      obelisk-selftest = self.callCabal2nix "obelisk-selftest" (cleanSource ./lib/selftest) {};
      obelisk-snap = self.callCabal2nix "obelisk-snap" (cleanSource ./lib/snap) {};
      obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (cleanSource ./lib/snap-extras) {};
    };

    defaultHaskellOverrides = composeExtensions fixUpstreamPkgs addLibs;
in
with pkgs.lib;
rec {
  inherit reflex-platform;
  inherit (reflex-platform) nixpkgs;
  path = reflex-platform.filterGit ./.;
  command = ghcObelisk.obelisk-command;
  selftest = pkgs.writeScript "selftest" ''
    #!/usr/bin/env bash
    set -euo pipefail

    PATH="${ghcObelisk.obelisk-command}/bin:$PATH"
    export OBELISK_IMPL="${hackGet ./.}"
    "${justStaticExecutables ghcObelisk.obelisk-selftest}/bin/obelisk-selftest"
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
  serverExe = exe: assets: config:
    pkgs.runCommand "serverExe" {} ''
      mkdir $out
      set -eux
      ln -s "${exe}"/* $out/
      ln -s $out/ghc/backend/bin/backend $out/
      ln -s $out/ghcjs/frontend/bin/frontend.jsexe $out/
      ln -s "${assets}" $out/static
      ln -s "${config}" $out/config
    '';
  server = exe: hostName:
    let system = "x86_64-linux";
        nixos = import (pkgs.path + /nixos);
        cfg = args: {
        };
    in nixos {
      inherit system;
      configuration = args: {
        imports = [
          (pkgs.path + /nixos/modules/virtualisation/amazon-image.nix)
        ];
        networking = { inherit hostName; };
        systemd.services.backend = {
          wantedBy = [ "multi-user-target" ];
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
        projectOut = reflex-platform.project (args@{ nixpkgs, ... }:
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
                  };
                  totalOverrides = composeExtensions (composeExtensions defaultHaskellOverrides projectOverrides) overrides;
                  ghcDevPackages = ["obelisk-run-frontend"];
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
    in projectOut //
         { server = { hostName }:
             server (serverExe projectOut.all assets.symlinked configPath) hostName;
         };
  haskellPackageSets = {
    ghc = reflex-platform.ghc.override {
      overrides = defaultHaskellOverrides;
    };
  };
}

{ system ? builtins.currentSystem }:
let reflex-platform = import ./reflex-platform { inherit system; };
    pkgs = reflex-platform.nixpkgs;
    # The haskell environment used to build Obelisk itself, e.g. the 'ob' command
    ghcObelisk = reflex-platform.ghc.override {
      overrides = self: super: with pkgs.haskell.lib; {
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
        #TODO: Put zsh completions in $out/share/zsh/vendor-completions and other completions in similar places
        obelisk-command = justStaticExecutables (self.callCabal2nix "obelisk-command" ./command {});

        optparse-applicative = self.callHackage "optparse-applicative" "0.14.0.0" {};
      };
    };
in
with pkgs.lib;
rec {
  inherit reflex-platform;
  command = ghcObelisk.obelisk-command;
  nullIfAbsent = p: if pathExists p then p else null;
  #TODO: Avoid copying files within the nix store.  Right now, obelisk-asset-manifest-generate copies files into a big blob so that the android/ios static assets can be imported from there; instead, we should get everything lined up right before turning it into an APK, so that copies, if necessary, only exist temporarily.
  processAssets = { src, packageName ? "static", moduleName ? "Static" }: pkgs.runCommand "asset-manifest" {
    inherit src;
    outputs = [ "out" "haskellManifest" "symlinked" ];
    buildInputs = [
      (reflex-platform.ghc.callCabal2nix "obelisk-asset-manifest" ./asset/manifest {})
    ];
  } ''
    set -euo pipefail
    touch "$out"
    obelisk-asset-manifest-generate "$src" "$haskellManifest" ${packageName} ${moduleName} "$symlinked"
  '';
  # An Obelisk project is a reflex-platform project with a predefined layout and role for each component
  project =
    { base ? ./..
    , android ? null #TODO: Better error when missing
    , ios ? null #TODO: Better error when missing
    }: reflex-platform.project ({ nixpkgs, ... }: with nixpkgs.haskell.lib;
    let frontendName = "frontend";
        backendName = "backend";
        commonName = "common";
        staticName = "static";
        staticPath = base + "/static";
        assets = processAssets { src = base + "/static"; };
        packages = filterAttrs (_: x: x != null) {
          ${frontendName} = nullIfAbsent (base + "/frontend");
          ${commonName} = nullIfAbsent (base + "/common");
          ${backendName} = nullIfAbsent (base + "/backend");
          obelisk-asset-serve = ./asset/serve;
          obelisk-asset-manifest = ./asset/manifest;
          obelisk-backend = ./backend;
        };
        overrides = self: super: {
          heist = doJailbreak super.heist; #TODO: Move up to reflex-platform; create tests for r-p supported packages
          ${staticName} = dontHaddock (self.callCabal2nix "static" assets.haskellManifest {});
        };
    in {
      inherit packages overrides;
      shells = {
        ghc = filter (x: hasAttr x packages) [
          backendName
          commonName
          frontendName
        ];
        ghcjs = filter (x: hasAttr x packages) [
          frontendName
          commonName
        ];
      };
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
    });
}

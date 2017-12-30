{ system ? builtins.currentSystem }:
let reflex-platform = import ./reflex-platform { inherit system; };
    pkgs = reflex-platform.nixpkgs;
in
with pkgs.lib;
rec {
  inherit reflex-platform;
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

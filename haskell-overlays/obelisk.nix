# Add obelisk packages

self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  inherit (pkgs) obeliskCleanSource;
  haskellLib = pkgs.haskell.lib;
  onLinux = pkg: f: if pkgs.stdenv.isLinux then f pkg else pkg;
in

{
  obelisk-executable-config-inject = pkgs.obeliskExecutableConfig.platforms.web.inject self;

  obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" (obeliskCleanSource ../lib/asset/manifest) {};
  obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" (obeliskCleanSource ../lib/asset/serve-snap) {};
  obelisk-backend = self.callCabal2nix "obelisk-backend" (obeliskCleanSource ../lib/backend) {};
  obelisk-command = haskellLib.overrideCabal (self.callCabal2nix "obelisk-command" (obeliskCleanSource ../lib/command) {}) {
    librarySystemDepends = [
      pkgs.jre
      pkgs.git
      pkgs.nixpkgs_unstable.nixVersions.nix_2_13
      pkgs.nix-prefetch-git
      pkgs.openssh
      pkgs.rsync
      pkgs.which
      (haskellLib.justStaticExecutables self.ghcid)
    ];
  };
  obelisk-frontend = self.callCabal2nix "obelisk-frontend" (obeliskCleanSource ../lib/frontend) {};
  obelisk-run = onLinux (self.callCabal2nix "obelisk-run" (obeliskCleanSource ../lib/run) {}) (pkg:
    haskellLib.overrideCabal pkg (drv: { librarySystemDepends = [ pkgs.iproute ]; })
  );
  obelisk-route = self.callCabal2nix "obelisk-route" (obeliskCleanSource ../lib/route) {};
  obelisk-selftest = haskellLib.dontHaddock (haskellLib.overrideCabal (super.callCabal2nix "obelisk-selftest" (obeliskCleanSource ../lib/selftest) { }) {
    librarySystemDepends = [
      pkgs.cabal-install
      pkgs.coreutils
      pkgs.git
      pkgs.nixpkgs_unstable.nixVersions.nix_2_13
      pkgs.nix-prefetch-git
      pkgs.rsync
    ];
  });
  obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (obeliskCleanSource ../lib/snap-extras) {};
  tabulation = self.callCabal2nix "tabulation" (obeliskCleanSource ../lib/tabulation) {};
}

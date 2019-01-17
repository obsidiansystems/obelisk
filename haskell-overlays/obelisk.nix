# Add obelisk packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  inherit (pkgs) cleanHaskellSource;
in

{
  obelisk-executable-config = pkgs.obeliskExecutableConfig.haskellPackage self;
  obelisk-executable-config-inject = pkgs.obeliskExecutableConfig.platforms.web.inject self;

  obelisk-asset-manifest = self.callCabal2nix "obelisk-asset-manifest" ../lib/asset/manifest {};
  obelisk-asset-serve-snap = self.callCabal2nix "obelisk-asset-serve-snap" ../lib/asset/serve-snap {};
  obelisk-backend = self.callCabal2nix "obelisk-backend" (cleanHaskellSource ../lib/backend) {};
  obelisk-cliapp = self.callCabal2nix "obelisk-cliapp" (cleanHaskellSource ../lib/cliapp) {};
  obelisk-command = self.callCabal2nix "obelisk-command" (cleanHaskellSource ../lib/command) {};
  obelisk-frontend = self.callCabal2nix "obelisk-frontend" (cleanHaskellSource ../lib/frontend) {};
  obelisk-run = self.callCabal2nix "obelisk-run" (cleanHaskellSource ../lib/run) {};
  obelisk-route = self.callCabal2nix "obelisk-route" (cleanHaskellSource ../lib/route) {};
  obelisk-selftest = self.callCabal2nix "obelisk-selftest" (cleanHaskellSource ../lib/selftest) {};
  obelisk-snap-extras = self.callCabal2nix "obelisk-snap-extras" (cleanHaskellSource ../lib/snap-extras) {};
}

let 
  obelisk = import ./default.nix {};
  pkgs = obelisk.pkgs;
  make-test = import (pkgs.path + /nixos/tests/make-test.nix); 
  selftest = pkgs.writeShellScriptBin "selftest" ''
    ${obelisk.selftest}
  '';
  obelisk-everything = (import ./release.nix { cacheBuildSystems = [ "x86_64-linux" ]; }).metaCache;
in
  make-test ({...}: {
    name  = "obelisk";
    nodes = {
      client = {
        imports = [
          (pkgs.path + /nixos/modules/installer/cd-dvd/channel.nix)
        ];
        nix.useSandbox = false;
        nix.binaryCaches = [ ];
        nix.binaryCachePublicKeys = [ ];
        environment.systemPackages = [
          obelisk.command
          obelisk.shell
          obelisk-everything
          selftest
          pkgs.git 
        ];
      };
    };

    testScript = ''
      startAll;

      $client->succeed("nix-instantiate --dry-run '<nixpkgs>' -A hello");

      $client->succeed("ob --help");
      # TODO: Link not found
      # obelisk unit tests
      $client->succeed("selftest -v");
     '';
   })

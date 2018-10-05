{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
, baseUrl ? "/"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
let
obApp = project ./. ({ ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";
});

server = { exe, hostName, adminEmail, routeHost, enableHttps }@args:
  let
    nixos = import (nixpkgs.path + /nixos);
  in nixos {
    system = "x86_64-linux";
    configuration = {
      imports = [
        (serverModules.mkBaseEc2 args)
        (serverModules.mkObeliskApp (args // { inherit baseUrl; }))
      ];
    };
  };
in obApp // { inherit server; }

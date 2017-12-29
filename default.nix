{}:
let reflex-platform = import ./reflex-platform {};
in
with reflex-platform.nixpkgs.lib;
rec {
  inherit reflex-platform;
  nullIfAbsent = p: if pathExists p then p else null;
  # An Obelisk project is a reflex-platform project with a predefined layout and role for each component
  project =
    { base ? ./..
    , android ? null #TODO: Better error when missing
    , ios ? null #TODO: Better error when missing
    }: reflex-platform.project ({ ... }:
    let frontendName = "frontend";
        backendName = "backend";
        commonName = "common";
        packages = filterAttrs (_: x: x != null) {
          ${frontendName} = nullIfAbsent (base + "/frontend");
          ${commonName} = nullIfAbsent (base + "/common");
          ${backendName} = nullIfAbsent (base + "/backend");
        };
    in {
      inherit packages;
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
        } // android;
      };
      ios = {
        ${if ios == null then null else frontendName} = {
          executableName = "frontend";
        } // ios;
      };
    });
}

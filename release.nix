{
  supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let 
  obelisk-everywhere = import ./everywhere.nix { cacheBuildSystems = supportedPlatforms; };
in obelisk-everywhere // {
  test = import ./test.nix;
}

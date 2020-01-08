{
  supportedPlatforms ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let 
  obelisk-everywhere = (import ./everywhere.nix { cacheBuildSystems = supportedPlatforms; }).metaCache;
  obelisk-test = import ./test.nix;
in {
  build = obelisk-everywhere;
  test = obelisk-test;
}

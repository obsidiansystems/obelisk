{
  supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
{ 
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
}

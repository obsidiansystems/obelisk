{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, reflex-platform-func ? import ./dep/reflex-platform
}:
let
  nameValuePair = name: value: { inherit name value; };
  genAttrs = name: f: builtins.listToAttrs (builtins.map (n: nameValuePair n (f n)) name);
  forEachSystem = genAttrs [ "x86_64-linux" "x86_64-darwin" ];
  pkgs = forEachSystem (system: (reflex-platform-func { system = system; }).nixpkgs);
in
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
  lint = forEachSystem (system: pkgs.${system}.runCommand "nixpkgs-fmt"
    {
      buildInputs = [ pkgs.${system}.nixpkgs-fmt ];
    } ''nixpkgs-fmt ./ && echo test > $out'');
  inherit (build) metaCache;
}

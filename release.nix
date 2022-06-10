{ supportedSystems ? [ "x86_64-linux" ]
, reflex-platform-func ? import ./dep/reflex-platform
}:
let
  nameValuePair = name: value: { inherit name value; };
  genAttrs = name: f: builtins.listToAttrs (builtins.map (n: nameValuePair n (f n)) name);
  forEachSystem = genAttrs [ "x86_64-linux" ];
  pkgs = forEachSystem (system: (reflex-platform-func { system = system; }).nixpkgs);
in
rec {
  recurseForDerivations = true;
  build = import ./all-builds.nix { inherit supportedSystems; };
  test = import ./all-tests.nix { inherit supportedSystems; };
  lint-script = forEachSystem (system:
    pkgs.${system}.writeShellScriptBin "lint-script" ''
      ln -s ${./.} $out
      ${pkgs.${system}.nixpkgs-fmt}/bin/nixpkgs-fmt $out --check
    '');
  lint = forEachSystem (system:
    pkgs.${system}.runCommandLocal "lint"
      {
        buildInputs = [ lint-script.${system} ];
      } "${lint-script.${system}}/bin/lint-script");
  inherit (build) metaCache;
}

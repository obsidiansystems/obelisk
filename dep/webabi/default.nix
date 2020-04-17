{ nodePackages, git, pkgs, system }:

let
  runNode2nix = name: src: pkgs.runCommand "${name}-nix" { nativeBuildInputs = [nodePackages.node2nix]; } ''
    mkdir -p $out
    cd $out
    node2nix -i ${src}/package.json -l ${src}/package-lock.json --include-peer-dependencies --nodejs-10 -d -o ./node-packages.nix -e ./node-env.nix -c ./default.nix
  '';

  callNode2nix = name: src: import (runNode2nix name src) { inherit pkgs system; };

  webabiSrc = pkgs.fetchFromGitHub {
    owner = "WebGHC";
    repo = "webabi";
    rev = "93b2099a2186f5e9b9eab4b0e07bad5ce03b6ee6";
    sha256 = "1d76xwq1p6479q06vqh0vfwnnnxravz19i1r0278c9i02n6ivcbd";
  };

in (callNode2nix "webabi" webabiSrc).package.overrideAttrs (old: {
  postInstall = ''
    ${old.postInstall or ""}
    patchShebangs .
    npm run build
  '';
})

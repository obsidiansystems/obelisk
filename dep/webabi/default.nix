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
    rev = "bd64537cbbc308d062a23ff81a172e74a5364396";
    sha256 = "1sg7h70rbiw75zmrzjnrnvzzw0nvzgr8pdn29jc5mgm2yqjkj8p7";
  };

in (callNode2nix "webabi" webabiSrc).package.overrideAttrs (old: {
  postInstall = ''
    ${old.postInstall or ""}
    patchShebangs .
    npm run build
  '';
})

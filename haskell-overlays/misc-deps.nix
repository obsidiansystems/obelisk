# Fix misc upstream packages
self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
in

{
  hnix = pkgs.haskell.lib.dontCheck (self.callCabal2nix "hnix" (pkgs.fetchFromGitHub {
    owner = "haskell-nix";
    repo = "hnix";
    rev = "42afdc21da5d9e076eab57eaa42bfdde938192b8";
    sha256 = "0psw384dx9bw2dp93xrzw8rd9amvcwgzn64jzzwby7sfspj6k349";
  }) {});
  # Need 8.0.2 build support
  # PR: https://github.com/dmwit/universe/pull/33
  universe-template = self.callCabal2nix "universe-template" (pkgs.fetchFromGitHub {
    owner = "obsidiansystems";
    repo = "universe";
    rev = "6a71119bfa5db2b9990a2491c941469ff8ef5d13";
    sha256 = "0z8smyainnlzcglv3dlx6x1n9j6d2jv48aa8f2421iayfkxg3js5";
  } + /template) {};
}

{ pkgs, nix-haskell-libs, nix-haskell-compilers, ... }:

let cabal = import "${nix-haskell-libs}/cabal.nix" { inherit pkgs; };

in {

  imports = [
    (import "${nix-haskell-compilers}/ghc-wasm-meta" {
      flavour = "9.12";
      drivers = [ "nixpkgs" ];
    })
  ];

  name = "obelisk";
  src = ./.;

  cabalProject = cabal.inline-cabal-project ./. "cabal.project";

  source-repository-packages = {
    reflex-dom = ../deps/reflex-dom/reflex-dom;
    reflex-dom-core = ../deps/reflex-dom/reflex-dom-core;
    chrome-test-utils = ../deps/reflex-dom/chrome-test-utils;
  };

  nixpkgs.options.use-plan = true;

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs wasi32 ];
    withHoogle = true;
  };

}

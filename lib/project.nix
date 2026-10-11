{ pkgs, nix-haskell-libs, nix-haskell-compilers, ... }:

let cabal = import "${nix-haskell-libs}/cabal.nix" { inherit pkgs; };

    # jsaddle-warp carrying the batch-pump stall fix
    # (https://github.com/ghcjs/jsaddle/pull/172) until upstream releases it.
    jsaddle-warp-src = builtins.fetchGit {
      url = "https://github.com/ghcjs/jsaddle.git";
      rev = "2152422657602dc29c89140cebd3d7fae080acf1";
    };
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
    jsaddle-warp = jsaddle-warp-src + "/jsaddle-warp";
  };

  nixpkgs.options.use-plan = true;

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs wasi32 ];
    withHoogle = true;
  };

}

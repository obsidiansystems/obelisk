{ pkgs, obeliskLib, ... }:

{

  name = "obelisk-skeleton";
  src = ./.;

  inherit (obeliskLib) source-repository-packages;

  obelisk.static.path = import ./static { inherit pkgs; };
  # For projects without a build step, use:
  # obelisk.static.path = ./static/src;

  shell = {
    crossPlatforms = ps: with ps; [
      wasi32

      # To enable JS builds in `nix-shell`, uncomment ghcjs below and use:
      #   cabal build/repl/run backend -f -wasm
      #   ob-run/ob-repl -f -wasm
      # ghcjs
    ];
    withHoogle = true;
  };

  # if you're not in a hurry
  #optimizations.all = true;

}

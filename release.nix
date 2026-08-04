{ system ? builtins.currentSystem, inputs ? {} }:

let skeleton = import ./skeleton { inherit system inputs; };

    haskell-nix = {
      shell = skeleton.haskell-nix.shell;
      serverExe-wasm = skeleton.haskell-nix.serverExe.wasm;
      serverExe-js = skeleton.haskell-nix.serverExe.js;
      containerImage-wasm = skeleton.haskell-nix.containerImage.wasm;
      containerImage-js = skeleton.haskell-nix.containerImage.js;
    };

    nixpkgs = {
      shell = skeleton.nixpkgs.shell;
      serverExe-js = skeleton.nixpkgs.serverExe.js;
      containerImage-js = skeleton.nixpkgs.containerImage.js;
    };

    entries = prefix: set:
      map (name: { name = "${prefix}-${name}"; path = set.${name}; })
        (builtins.attrNames set);

in {
  inherit haskell-nix nixpkgs;

  all = skeleton.pkgs.linkFarm "obelisk-release"
    (entries "haskell-nix" haskell-nix ++ entries "nixpkgs" nixpkgs);
}

{ system ? builtins.currentSystem, inputs ? {} }:

let skeleton = import ./skeleton { inherit system inputs; };

in skeleton.nixpkgs.linkFarm "obelisk-release" [
  { name = "shell"; path = skeleton.shell; }
  { name = "serverExe-wasm"; path = skeleton.serverExe.wasm; }
  { name = "serverExe-js"; path = skeleton.serverExe.js; }
  { name = "containerImage-wasm"; path = skeleton.containerImage.wasm; }
  { name = "containerImage-js"; path = skeleton.containerImage.js; }
]

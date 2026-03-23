{ system ? builtins.currentSystem }:

let project = import ./default.nix { inherit system; };

in project.shell

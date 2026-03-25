{ system ? builtins.currentSystem, inputs ? {} }:

let project = import ./default.nix { inherit system inputs; };

in project.shell

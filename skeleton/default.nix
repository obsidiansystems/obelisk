{ system ? builtins.currentSystem, inputs ? {} }:

let obelisk = import ./deps/obelisk { inherit system inputs; };

in obelisk.project (import ./project.nix)

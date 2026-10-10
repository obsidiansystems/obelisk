{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix
}:

let obelisk = import inputs.obelisk { inherit system inputs; };

in obelisk.project (import ./project.nix)

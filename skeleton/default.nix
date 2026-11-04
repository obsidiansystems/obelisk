{ system ? builtins.currentSystem
, inputs ? import ./inputs.nix { inherit system; }
}:

let obelisk = import inputs.obelisk { inherit system inputs; };

in obelisk.project (import ./project.nix)

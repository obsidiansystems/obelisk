{ system ? builtins.currentSystem }:
let obelisk = import ./deps/obelisk { inherit system; };
in obelisk.project (import ./project.nix)

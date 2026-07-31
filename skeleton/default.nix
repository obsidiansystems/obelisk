{ system ? builtins.currentSystem, inputs ? {} }:

# Two ways to reach obelisk:
#   * as a flake input (see flake.nix): in a scaffolded project, or whenever
#     this file is imported with flake inputs;
#   * via the deps/obelisk symlink: in the obelisk repo (points at the repo
#     root) and in --link scaffolds (points at a local obelisk checkout);
#     legacy nix-shell uses this, tracking the live working tree.
let inputs' =
      if inputs == {} && !builtins.pathExists ./deps/obelisk
      then import ./inputs.nix
      else inputs;

    obeliskSrc =
      if inputs' ? obelisk
      then inputs'.obelisk
      else ./deps/obelisk;

    obelisk = import obeliskSrc { inherit system; inputs = inputs'; };

in obelisk.project (import ./project.nix)

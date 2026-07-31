{ system ? builtins.currentSystem, inputs ? {} }:

# obelisk lives at deps/obelisk: a git submodule in a scaffolded project, and
# a symlink in the obelisk repo (pointing at the repo root) and in --link
# scaffolds (pointing at a local obelisk checkout, so it tracks that working
# tree). Flake inputs win when present; flake.nix passes inputs.obelisk, which
# is itself a path to deps/obelisk.
let obeliskSrc =
      if inputs ? obelisk
      then inputs.obelisk
      else ./deps/obelisk;

    obelisk = import obeliskSrc { inherit system inputs; };

in obelisk.project (import ./project.nix)

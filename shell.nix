{ system ? builtins.currentSystem, inputs ? {} }:

# Obelisk's dev shell is the skeleton project's shell: it carries the GHC and
# cross toolchains plus the ob-* scripts, and developing obelisk means
# building/running the skeleton against the local lib/ sources.
import ./skeleton/shell.nix { inherit system inputs; }

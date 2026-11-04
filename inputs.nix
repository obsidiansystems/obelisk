# Example:
#   import ./inputs.nix { system = "x86_64-linux"; }
#   => { haskell-nix = <source>; nixpkgs = <source>; sandstone = <source>; pkgs = <package set>; }
#
#   import ./inputs.nix { system = "x86_64-linux"; overrides = inputs; }  # in flake.nix
#   => the same shape, with each flake input in place of the entry of the same name
#
#   import ./inputs.nix { system = "x86_64-linux"; overrides = { pkgs = myPkgs; }; }
#   => the same sources, with myPkgs as pkgs
{ system ? builtins.currentSystem
, overrides ? {}
}:

let

    # ------------------------------------------------------------------------
    # Configuration
    # ------------------------------------------------------------------------

    # Directories of thunks and git submodules. Each replaces the flake.lock entry of the same name.
    thunkDirs = [ ./deps ];

    # The package set for every consumer.
    mkPkgs = sources: import sources.nixpkgs {
      inherit system;
      overlays = builtins.attrValues (import ./nix/overlays.nix);
    };

    # ------------------------------------------------------------------------
    # Flake inputs
    # ------------------------------------------------------------------------

    src =
      if builtins.pathExists ./.git
      then builtins.fetchGit { url = ./.; submodules = true; }
      else { outPath = ./.; };

    lock = builtins.fromJSON (builtins.readFile ./flake.lock);

    nodeName = ref:
      if builtins.isList ref
      then builtins.foldl' (name: input: nodeName lock.nodes.${name}.inputs.${input}) lock.root ref
      else ref;

    flakeCompat = lock.nodes.${nodeName lock.nodes.${lock.root}.inputs.flake-compat}.locked;

    lockedInputs =
      if builtins.pathExists ./flake.lock
      then
        (import
          (fetchTarball {
            url = "https://github.com/${flakeCompat.owner}/${flakeCompat.repo}/archive/${flakeCompat.rev}.tar.gz";
            sha256 = flakeCompat.narHash;
          }) { inherit src; }
        ).outputs.inputs
      else {};

    flakeSources = inputs: builtins.mapAttrs (_: v: v.src or v) inputs;

    # ------------------------------------------------------------------------
    # Thunks
    # ------------------------------------------------------------------------

    linkedCheckout = path: toString path + "/.";

    linkTarget = path:
      if builtins.pathExists (path + "/.git")
      then builtins.fetchGit { url = linkedCheckout path; submodules = true; }
      else builtins.path { inherit path; };

    thunkSource = path:
      if builtins.pathExists (path + "/thunk.nix")
      then import (path + "/thunk.nix")
      else if builtins.readFileType path == "symlink" || builtins.pathExists (path + "/.git")
      then linkTarget path
      else path;

    thunkSourcesIn = dir:
      let entries = builtins.readDir dir;
          trees = builtins.filter
            (name: builtins.elem entries.${name} [ "directory" "symlink" ])
            (builtins.attrNames entries);
          entryFor = name: {
            inherit name;
            value = thunkSource (dir + "/${name}");
          };
      in builtins.listToAttrs (map entryFor trees);

    thunkSources = builtins.foldl' (srcs: dir: srcs // thunkSourcesIn dir) {} thunkDirs;

    # ------------------------------------------------------------------------
    # Result
    # ------------------------------------------------------------------------

    sources = flakeSources lockedInputs // thunkSources // flakeSources overrides;

in sources // { pkgs = overrides.pkgs or (mkPkgs sources); }

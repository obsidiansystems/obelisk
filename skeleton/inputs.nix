let thunks = [ ./deps ];

    src =
      if builtins.pathExists ./.git
      then builtins.fetchGit { url = ./.; submodules = true; }
      else { outPath = ./.; };

    flakeInputs =
      let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
          nodeName = ref:
            if builtins.isList ref
            then builtins.foldl' (name: input: nodeName lock.nodes.${name}.inputs.${input}) lock.root ref
            else ref;
          flakeCompat = lock.nodes.${nodeName lock.nodes.${lock.root}.inputs.flake-compat}.locked;
      in if builtins.pathExists ./flake.lock
      then
        (import
          (fetchTarball {
            url = "https://github.com/${flakeCompat.owner}/${flakeCompat.repo}/archive/${flakeCompat.rev}.tar.gz";
            sha256 = flakeCompat.narHash;
          }) { inherit src; }
        ).outputs.inputs
      else {};

    flakeSrcs = builtins.mapAttrs (_: v: v.src or v) flakeInputs;

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
    thunkSources = dir:
      let entries = builtins.readDir dir;
          trees = builtins.filter
            (name: builtins.elem entries.${name} [ "directory" "symlink" ])
            (builtins.attrNames entries);
          entryFor = name: {
            inherit name;
            value = thunkSource (dir + "/${name}");
          };
      in builtins.listToAttrs (map entryFor trees);

    thunkSrcs = builtins.foldl' (srcs: dir: srcs // thunkSources dir) {} thunks;

in flakeSrcs // thunkSrcs

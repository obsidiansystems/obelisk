let src =
      if builtins.pathExists ./.git
      then builtins.fetchGit { url = ./.; submodules = true; }
      else { outPath = ./.; };

    flakeInputs = (import
      (let lock = builtins.fromJSON (builtins.readFile ./flake.lock);
       in fetchTarball {
         url = "https://github.com/${lock.nodes.flake-compat.locked.owner}/${lock.nodes.flake-compat.locked.repo}/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
         sha256 = lock.nodes.flake-compat.locked.narHash;
       }) { inherit src; }
    ).outputs.inputs;

    flakeSrcs = builtins.mapAttrs (_: v: v.src or v) flakeInputs;

    linkTarget = path:
      if builtins.pathExists (path + "/.git")
      then builtins.fetchGit { url = path; submodules = true; }
      else builtins.path { inherit path; };
    thunkSource = path:
      if builtins.pathExists (path + "/thunk.nix")
      then import (path + "/thunk.nix")
      else if builtins.readFileType path == "symlink"
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

    thunkSrcs = thunkSources ./deps;

in flakeSrcs // thunkSrcs

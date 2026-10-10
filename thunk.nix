let root = (import ./. {}).src;
in if builtins.pathExists (root + "/.git")
   then builtins.fetchGit { url = root; submodules = true; }
   else root

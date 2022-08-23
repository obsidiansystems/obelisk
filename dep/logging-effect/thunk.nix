# DO NOT HAND-EDIT THIS FILE
let fetch = { private ? false, fetchSubmodules ? false, owner, repo, rev, sha256, ... }:
  if !fetchSubmodules && !private then builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; inherit sha256;
  } else (import "/nix/store/qjg458n31xk1l6lj26c3b871d4i4is98-source" {}).fetchFromGitHub {
    inherit owner repo rev sha256 fetchSubmodules private;
  };
  json = builtins.fromJSON (builtins.readFile ./github.json);
in fetch json

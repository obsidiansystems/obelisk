# DO NOT HAND-EDIT THIS FILE
let fetch = {url, rev, branch ? null, sha256 ? null, fetchSubmodules ? false, private ? false, ...}:
  let realUrl = let firstChar = builtins.substring 0 1 url; in
    if firstChar == "/" then /. + url
    else if firstChar == "." then ./. + url
    else url;
  in if !fetchSubmodules && private then builtins.fetchGit {
    url = realUrl; inherit rev;
    ${if branch == null then null else "ref"} = branch;
  } else (import (builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/3aad50c30c826430b0270fcf8264c8c41b005403.tar.gz";
  sha256 = "0xwqsf08sywd23x0xvw4c4ghq0l28w2ki22h0bdn766i16z9q2gr";
}) {}).fetchgit {
    url = realUrl; inherit rev sha256;
  };
  json = builtins.fromJSON (builtins.readFile ./git.json);
in fetch json
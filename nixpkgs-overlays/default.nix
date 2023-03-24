self: super:

let
  inherit (self) lib;
  inherit (import ../dep/gitignore.nix { inherit lib; }) gitignoreSource;
in {
  obeliskCleanSource = src:
    # WARNING: The order of application here seems to matter a great deal to
    # how quickly `ghcid` is able to reload changes. As a rule of thumb,
    # always apply `gitignoreSource` first.
    # See https://github.com/obsidiansystems/obelisk/pull/666 and related.
    lib.cleanSource (gitignoreSource src);

  obeliskExecutableConfig = self.callPackage ../lib/executable-config {};

  nixpkgs_unstable = import ../dep/nixpkgs_unstable { };
}

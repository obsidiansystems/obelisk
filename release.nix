{ local-self ? import ./. {}
, local-pkgs ? local-self.nixpkgs
}:

with local-pkgs.lib;

genAttrs [ "x86_64-linux" "x86_64-darwin" ] (system: {
  inherit (import ./. { inherit system; }) command;
  built-skeleton = (import ./skeleton { inherit system; }).all;
  tests = {
    #TODO: Doesn't work; see discussion in https://www.pivotaltracker.com/story/show/157265140
    # ob-init = runCommand "ob-init" {
    #  nativeBuildInputs = [
    #    self.command
    #    nix
    #  ];
    # } ''
    #  # mkdir $out
    #  # cd $out
    #  # ob init --symlink=${self.path}
    #'';

    #TODO: Re-enable this
    #verify-migration = runCommand "verify-migration" {
    # nativeBuildInputs = [
    #   self.command
    #   nix  # obelisk uses `nix-hash`
    # ];
    #} ''
    # set -e
    # # Fix permissions so `removePathForcibly` works on copied-to-tmp paths
    # cp -a ${self.pathGit} $TMPDIR/obelisk
    # cd $TMPDIR/obelisk
    # chmod -R u+w .
    #
    # (set -x;
    #  ob internal hash;
    #  ob internal verify-migration;
    # )
    #
    # mkdir -p $out
    # touch $out/done
    #'';
  };
})

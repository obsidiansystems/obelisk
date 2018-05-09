{ self ? import ./. {}
, pkgs ? self.nixpkgs
}:

with pkgs;

{
  tests = {
    #TODO: Doesn't work; see discussion in https://www.pivotaltracker.com/story/show/157265140
    ob-init = runCommand "ob-init" {
      nativeBuildInputs = [
        self.command
        nix
      ];
    } ''
      mkdir $out
      cd $out
      ob init --symlink=${self.path}
    '';
  };
}

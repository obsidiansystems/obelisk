self: super:
let
  pkgs = self.callPackage ({ pkgs }: pkgs) { };
  haskellLib = pkgs.haskell.lib;

  commandRuntimeDeps = with pkgs; [
    coreutils
    git
    nix-prefetch-git
    openssh
  ];
in
{
  # Dynamic linking with split objects dramatically increases startup time (about
  # 0.5 seconds on a decent machine with SSD), so we do `justStaticExecutables`.
  obelisk-command = haskellLib.overrideCabal
    (haskellLib.generateOptparseApplicativeCompletion "ob"
      (haskellLib.justStaticExecutables super.obelisk-command))
    (drv: {
      buildTools = (drv.buildTools or [ ]) ++ [ pkgs.buildPackages.makeWrapper ];
      postFixup = ''
        ${drv.postFixup or ""}
        # Make `ob` reference its runtime dependencies.
        wrapProgram "$out"/bin/ob --prefix PATH : ${pkgs.lib.makeBinPath commandRuntimeDeps}
      '';
      passthru = { inherit commandRuntimeDeps; };
    });

  obelisk-selftest = haskellLib.justStaticExecutables super.obelisk-selftest;
}

self: super:

let
  pkgs = self.callPackage ({ pkgs }: pkgs) {};
  haskellLib = pkgs.haskell.lib;
  #TODO: Upstream
  # Modify a Haskell package to add completion scripts for the given
  # executable produced by it.  These completion scripts will be picked up
  # automatically if the resulting derivation is installed, e.g. by
  # `nix-env -i`.
  addOptparseApplicativeCompletionScripts = exeName: pkg: haskellLib.overrideCabal pkg (drv: {
    postInstall = (drv.postInstall or "") + ''
    BASH_COMP_DIR="$out/share/bash-completion/completions"
    mkdir -p "$BASH_COMP_DIR"
    "$out/bin/${exeName}" --bash-completion-script "$out/bin/${exeName}" >"$BASH_COMP_DIR/ob"

    ZSH_COMP_DIR="$out/share/zsh/vendor-completions"
    mkdir -p "$ZSH_COMP_DIR"
    "$out/bin/${exeName}" --zsh-completion-script "$out/bin/${exeName}" >"$ZSH_COMP_DIR/_ob"

    FISH_COMP_DIR="$out/share/fish/vendor_completions.d"
    mkdir -p "$FISH_COMP_DIR"
    "$out/bin/${exeName}" --fish-completion-script "$out/bin/${exeName}" >"$FISH_COMP_DIR/ob.fish"
    '';
  });
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
  (addOptparseApplicativeCompletionScripts "ob"
  (haskellLib.justStaticExecutables super.obelisk-command))
  (drv: {
    buildTools = (drv.buildTools or []) ++ [ pkgs.buildPackages.makeWrapper ];
    postFixup = ''
    ${drv.postFixup or ""}
    # Make `ob` reference its runtime dependencies.
    wrapProgram "$out"/bin/ob --prefix PATH : ${pkgs.lib.makeBinPath commandRuntimeDeps}
    '';
    passthru = { inherit commandRuntimeDeps; };
  });

  obelisk-selftest = haskellLib.justStaticExecutables super.obelisk-selftest;
}

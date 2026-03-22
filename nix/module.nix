# nix-haskell module that wires obelisk overrides and hackage overlays
# into a project. Declares `obelisk.static` and `obelisk.frontend.js` options;
# when set, generates hackage overlay and wires assets into frontend/backend data dirs.
# frontend.js defaults to the project's GHCJS-cross-compiled frontend.
{ config, lib, system, nix-haskell-patches, ... }:

let obeliskLib = import ./lib.nix { inherit system; };

    static = config.obelisk.static;

    frontendJs = config.obelisk.frontend.js;

in {
  imports = [
    "${nix-haskell-patches}/js/splitmix"
  ];

  options.obelisk = {
    static = lib.mkOption {
      type = lib.types.nullOr (lib.types.either lib.types.path lib.types.package);
      default = null;
      description = "Static assets path or derivation.";
    };

    frontend.js = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = obeliskLib.frontendJs config;
      defaultText = lib.literalExpression "obeliskLib.frontendJs config";
      description = "GHCJS-compiled frontend derivation.";
    };
  };

  config = {
    inherit (obeliskLib) extraCabalProject;
    # source-repository-packages disabled; using optional-packages in cabal.project instead.
    # inherit (obeliskLib) source-repository-packages;

    hackage-overlays =
      if static != null
      then [ (obeliskLib.obeliskGeneratedStaticOverlay static) ]
      else [];

    overrides = [
      obeliskLib.buildTypeOverride
      obeliskLib.jsexeOverride
      (obeliskLib.frontendDataOverride { inherit static; })
      (obeliskLib.backendDataOverride { inherit static; inherit frontendJs; })
    ];
  };
}

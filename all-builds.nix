{ self-args ? {
    config.android_sdk.accept_license = true;
    iosSdkVersion = "10.2";
  }
, local-self ? import ./. self-args
, supportedSystems ? [
    "x86_64-linux"
    "x86_64-darwin"
  ]
}:

let
  inherit (local-self.nixpkgs) lib runCommand nix;
  cacheBuildSystems = supportedSystems;

  obeliskPackagesCommon = [
    "obelisk-frontend"
    "obelisk-route"
    "obelisk-executable-config-lookup"
  ];

  obeliskPackagesBackend = obeliskPackagesCommon ++ [
    "obelisk-asset-manifest"
    "obelisk-asset-serve-snap"
    "obelisk-backend"
    "obelisk-cliapp"
    "obelisk-command"
    "obelisk-executable-config-inject"
    "obelisk-frontend"
    "obelisk-run"
    "obelisk-route"
    "obelisk-selftest"
    "obelisk-snap-extras"
  ];

  pnameToAttrs = pkgsSet: pnames:
    lib.listToAttrs (map
      (name: { inherit name; value = pkgsSet.${name}; })
      pnames);

  collect = v:
    if lib.isDerivation v then [v]
    else if lib.isAttrs v then lib.concatMap collect (builtins.attrValues v)
    else if lib.isList v then lib.concatMap collect v
    else [];

  perPlatform = lib.genAttrs cacheBuildSystems (system: let
    reflex-platform = import ./dep/reflex-platform { inherit system; };

    mkPerProfiling = profiling: let
      obelisk = import ./. (self-args // { inherit system profiling; });
      ghc = pnameToAttrs
        obelisk.haskellPackageSets.ghc
        obeliskPackagesBackend;
      ghcjs = pnameToAttrs
        obelisk.haskellPackageSets.ghcjs
        obeliskPackagesCommon;
      command = obelisk.command;
      skeleton = import ./skeleton { inherit obelisk; };
      serverSkeletonExe = skeleton.exe;
      # TODO fix nixpkgs so it doesn't try to run the result of haskell shells as setup hooks.
      serverSkeletonShell = local-self.nixpkgs.runCommand "shell-safe-for-dep" {} ''
        touch "$out"
        echo "return" >> "$out"
        cat "${skeleton.shells.ghc}" >> "$out"
      '';
      androidSkeleton = (import ./skeleton { inherit obelisk; }).android.frontend;
      iosSkeleton = (import ./skeleton { inherit obelisk; }).ios.frontend;
      nameSuffix = if profiling then "profiled" else "unprofiled";
      packages = {
        skeletonProfiledObRun = skeleton.__unstable__.profiledObRun;
        inherit
          command
          serverSkeletonShell
          ghc
          ;
      } // lib.optionalAttrs (!profiling) {
        inherit
          ghcjs
          serverSkeletonExe
          ;
      } // lib.optionalAttrs reflex-platform.androidSupport {
        inherit androidSkeleton;
      } // lib.optionalAttrs reflex-platform.iosSupport {
        inherit iosSkeleton;
      };
    in packages // {
      cache = reflex-platform.pinBuildInputs
        "obelisk-${system}-${nameSuffix}"
        # skeletonProfiledObRun is a binary, so can’t be used in pinBuildInputs
        (collect (builtins.removeAttrs ["skeletonProfiledObRun"] packages));
    };

    perProfiling = {
      profiled = mkPerProfiling true;
      unprofiled = mkPerProfiling false;
    };
  in perProfiling // {
    cache = reflex-platform.pinBuildInputs
      "obelisk-${system}"
      (map (p: p.cache) (builtins.attrValues perProfiling));
  });

  metaCache = local-self.reflex-platform.pinBuildInputs
    "obelisk-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }

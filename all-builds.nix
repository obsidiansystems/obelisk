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

  concatDepends = let
    extractDeps = x: (x.override {
      mkDerivation = drv: {
        out = builtins.concatLists [
          (drv.buildDepends or [])
          (drv.libraryHaskellDepends or [])
          (drv.executableHaskellDepends or [])
        ];
      };
    }).out;
  in pkgAttrs: builtins.concatLists (map extractDeps (builtins.attrValues pkgAttrs));

  perPlatform = lib.genAttrs cacheBuildSystems (system: let
    reflex-platform = import ./dep/reflex-platform { inherit system; };
    perProfiling = profiling: let
      obelisk = import ./. (self-args // { inherit system profiling; });
      ghc = pnameToAttrs
        obelisk.haskellPackageSets.ghc
        obeliskPackagesBackend;
      ghcjs = pnameToAttrs
        obelisk.haskellPackageSets.ghcjs
        obeliskPackagesCommon;
      cachePackages = builtins.concatLists [
        (builtins.attrValues ghc)
        (builtins.attrValues ghcjs)
        (concatDepends ghc)
        (concatDepends ghcjs)
        (lib.optional reflex-platform.androidSupport androidSkeleton)
        (lib.optional reflex-platform.iosSupport iosSkeleton)
        [ command serverSkeletonExe serverSkeletonShell ]
      ];
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
    in {
      inherit
        command
        ghc ghcjs
        serverSkeletonExe
        serverSkeletonShell
        ;
      cache = reflex-platform.pinBuildInputs "obelisk-${system}-${nameSuffix}" cachePackages;
    } // lib.optionalAttrs reflex-platform.androidSupport {
      inherit androidSkeleton;
    } // lib.optionalAttrs reflex-platform.iosSupport {
      inherit iosSkeleton;
    };
    profiled = perProfiling true;
    unprofiled = perProfiling false;
    cachePackages = map (p: p.cache) [ profiled unprofiled ];
  in {
    inherit profiled unprofiled;
    cache = reflex-platform.pinBuildInputs "obelisk-${system}" cachePackages;
  });

  metaCache = local-self.reflex-platform.pinBuildInputs
    "obelisk-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }

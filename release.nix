{ self-args ? {}
, local-self ? import ./. self-args
}:

let
  inherit (local-self.nixpkgs) lib runCommand nix;

  cacheBuildSystems = [ "x86_64-linux" "x86_64-darwin" ];

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
    obelisk = import ./. { inherit system; };
    reflex-platform = obelisk.reflex-platform;
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
      [ command serverExeSkeleton ]
    ];
    command = local-self.command;
    serverExeSkeleton = (import ./skeleton { obelisk = local-self; }).exe;
    androidObelisk = import ./. (self-args // {
      config.android_sdk.accept_license = true;
    });
    androidSkeleton = (import ./skeleton { obelisk = androidObelisk; }).android.frontend;
    iosObelisk = import ./. (self-args // {
      system = "x86_64-darwin";
      iosSdkVersion = "10.2";
    });
    iosSkeleton = (import ./skeleton { obelisk = iosObelisk; }).ios.frontend;
  in {
    inherit
      command
      ghc ghcjs
      serverExeSkeleton
      ;
    cache = reflex-platform.pinBuildInputs "obelisk-${system}" cachePackages;
  } // lib.optionalAttrs reflex-platform.androidSupport {
    inherit androidSkeleton;
  } // lib.optionalAttrs reflex-platform.iosSupport {
    inherit iosSkeleton;
  });

  metaCache = local-self.pinBuildInputs
    "obelisk-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in perPlatform // { inherit metaCache; }

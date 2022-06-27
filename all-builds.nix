{ self-args ? {
    config.android_sdk.accept_license = true;
    iosSdkVersion = "13.2";
  }
, local-self ? import ./. self-args
, supportedSystems ? [ builtins.currentSystem ]
, rp ? import ./dep/reflex-platform
, version ? "ghc-8.10.7"
}:

let
  inherit (local-self.nixpkgs) lib runCommand nix;
  cacheBuildSystems = supportedSystems;
  rp-new = rp;

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
    if lib.isDerivation v then [ v ]
    else if lib.isAttrs v then lib.concatMap collect (builtins.attrValues v)
    else if lib.isList v then lib.concatMap collect v
    else [ ];

  forceGhc810 = rp:
    let
      rp810 = rp // { ghc = rp.ghc8_10; ghcjs = rp.ghcjs8_10; };
    in
    rp810 // { project = args: import (./dep/reflex-platform + "/project") rp810 (args ({ pkgs = rp.nixpkgs; } // rp810)); };

  perPlatform = lib.genAttrs cacheBuildSystems (system:
    let
      reflex-platform =
        if version == "ghc-8.6.5" then
          rp { inherit system; }
        else
          forceGhc810 (import ./dep/reflex-platform { inherit system; });
      mkPerProfiling = profiling:
        let
          obelisk = import ./. (self-args // { inherit system profiling; });
          ghc = pnameToAttrs
            obelisk.haskellPackageSets.ghc
            obeliskPackagesBackend;
          ghcjs = pnameToAttrs
            obelisk.haskellPackageSets.ghcjs
            obeliskPackagesCommon;
          command = obelisk.command;

          withSkeletonOptions = skel: options: (skel.passthru.__unstable__.self.extend (self: super: {
            userSettings = super.userSettings // options;
          })).project;
          rawSkeleton = import ./skeleton { inherit obelisk; };
          skeleton = withSkeletonOptions rawSkeleton {
            withHoogle = true; # cache the Hoogle database for the skeleton
            __withGhcide = true; # cache the ghcide build for the skeleton
          };

          serverSkeletonExe = rawSkeleton.exe;
          # TODO fix nixpkgs so it doesn't try to run the result of haskell shells as setup hooks.
          serverSkeletonShell = local-self.nixpkgs.runCommand "shell-safe-for-dep" { } ''
            touch "$out"
            echo "return" >> "$out"
            cat "${skeleton.shells.ghc}" >> "$out"
          '';
          androidSkeleton = skeleton.android.frontend;
          iosSkeleton = skeleton.ios.frontend;
          nameSuffix = if profiling then "profiled" else "unprofiled";
          packages = {
            skeletonProfiledObRun = rawSkeleton.__unstable__.profiledObRun;
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
        in
        packages // {
          cache = reflex-platform.pinBuildInputs
            "obelisk-${system}-${nameSuffix}"
            (collect packages);
        };

      perProfiling = {
        profiled = mkPerProfiling true;
        unprofiled = mkPerProfiling false;
      };
    in
    perProfiling // {
      cache = reflex-platform.pinBuildInputs
        "obelisk-${system}"
        (map (p: p.cache) (builtins.attrValues perProfiling));
    });

  metaCache = local-self.reflex-platform.pinBuildInputs
    "obelisk-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));

in
perPlatform // { inherit metaCache; }

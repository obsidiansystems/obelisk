{ self-args ? {}
, local-self ? import ./. self-args
}:

let
  inherit (local-self.nixpkgs) lib runCommand nix;

  cacheBuildSystems = [ "x86_64-linux" "x86_64-darwin" ];

  obeliskPackagesCommon = [
    "obelisk-frontend"
    "obelisk-route"
    "obelisk-executable-config"
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
    obelisk = import ./. (self-args // { inherit system; });
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
    ];
    command = local-self.command;
    serverExeSkeleton = (import ./skeleton (self-args // { inherit system; })).exe;
    builtSkeletons = let skeleton = import ./skeleton (self-args // { inherit system; }); in {
      android = if skeleton.reflex.androidSupport then skeleton.android else {};
      ios = if skeleton.reflex.iosSupport then skeleton.ios else {};
    } // lib.mapAttrs (compiler: pkgList: lib.genAttrs pkgList (pkg: skeleton.${compiler}.${pkg})) { ghc = ["common" "frontend" "backend"]; ghcjs = ["common" "frontend"]; };
  in {
    inherit
      command
      ghc ghcjs
      serverExeSkeleton builtSkeletons;
    cache = reflex-platform.pinBuildInputs
      "obelisk-${system}"
      cachePackages
      ([command serverExeSkeleton] ++ lib.concatMap builtins.attrValues (builtins.attrValues builtSkeletons));
  });

  metaCache = local-self.pinBuildInputs
    "obelisk-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform))
    [];

in perPlatform // { inherit metaCache; }

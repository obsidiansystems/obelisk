{ nixpkgsFunc ? import ./nixpkgs
, system ? builtins.currentSystem
, config ? {}
, enableLibraryProfiling ? false
, enableExposeAllUnfoldings ? true
, enableTraceReflexEvents ? false
, useFastWeak ? true
, useReflexOptimizer ? false
, useTextJSString ? true
, __useLegacyCompilers ? false # Interface unstable
, iosSdkVersion ? "10.2"
, nixpkgsOverlays ? []
, haskellOverlays ? []
}:
let iosSupport = system == "x86_64-darwin";
    androidSupport = lib.elem system [ "x86_64-linux" ];

    # Overlay for GHC with -load-splices & -save-splices option
    splicesEval = self: super: {
      haskell = super.haskell // {
        compiler = super.haskell.compiler // {
          ghcSplices-8_4 = super.haskell.compiler.ghc843.overrideAttrs (drv: {
            enableParallelBuilding = false;
            patches = (drv.patches or [])
              ++ [ ./splices-load-save.patch ./haddock.patch ];
          });
        };
        packages = super.haskell.packages // {
          ghcSplices-8_4 = super.haskell.packages.ghc843.override {
            buildHaskellPackages = self.buildPackages.haskell.packages.ghcSplices-8_4;
            ghc = self.buildPackages.haskell.compiler.ghcSplices-8_4;
          };
        };
      };
    };

    bindHaskellOverlays = self: super: {
      haskell = super.haskell // {
        overlays = super.overlays or {} // import ./haskell-overlays {
          nixpkgs = self;
          inherit
            haskellLib
            fetchFromGitHub hackGet
            ghcjsBaseSrc ghcjsBaseTextJSStringSrc
            useFastWeak useReflexOptimizer enableLibraryProfiling enableTraceReflexEvents
            useTextJSString enableExposeAllUnfoldings
            stage2Script
            optionalExtension
            haskellOverlays;
          inherit ghcSavedSplices;
          inherit (self) lib;
          androidActivity = hackGet ./android-activity;
        };
      };
    };

    forceStaticLibs = self: super: {
      darwin = super.darwin // {
        libiconv = super.darwin.libiconv.overrideAttrs (_:
          lib.optionalAttrs (self.stdenv.hostPlatform != self.stdenv.buildPlatform) {
            postInstall = "rm $out/include/libcharset.h $out/include/localcharset.h";
            configureFlags = ["--disable-shared" "--enable-static"];
          });
      };
      zlib = super.zlib.override (lib.optionalAttrs
        (self.stdenv.hostPlatform != self.stdenv.buildPlatform)
        { static = true; });
    };

    mobileGhcOverlay = import ./nixpkgs-overlays/mobile-ghc { inherit lib; };

    nixpkgsArgs = {
      inherit system;
      overlays = [
        bindHaskellOverlays
        forceStaticLibs
        mobileGhcOverlay
        splicesEval
      ] ++ nixpkgsOverlays;
      config = {
        permittedInsecurePackages = [
          "webkitgtk-2.4.11"
        ];
        packageOverrides = pkgs: {
          webkitgtk = pkgs.webkitgtk220x;
        };

        # XCode needed for native macOS app
        # Obelisk needs it to for some reason
        allowUnfree = true;
      } // config;
    };

    nixpkgs = nixpkgsFunc nixpkgsArgs;

    inherit (nixpkgs) lib fetchurl fetchgit fetchgitPrivate fetchFromGitHub;

    nixpkgsCross = {
      android = lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        aarch64 = {
          crossSystem = lib.systems.examples.aarch64-android-prebuilt;
        };
        aarch32 = {
          crossSystem = lib.systems.examples.armv7a-android-prebuilt // {
            # Hard to find newer 32-bit phone to test with that's newer than
            # this. Concretely, doing so resulted in:
            # https://android.googlesource.com/platform/bionic/+/master/libc/arch-common/bionic/pthread_atfork.h#19
            sdkVer = "22";
          };
        };
        # Back compat
        arm64 = lib.warn "nixpkgsCross.android.arm64 has been deprecated, using nixpkgsCross.android.aarch64 instead." aarch64;
        armv7a = lib.warn "nixpkgsCross.android.armv7a has been deprecated, using nixpkgsCross.android.aarch32 instead." aarch32;
        arm64Impure = lib.warn "nixpkgsCross.android.arm64Impure has been deprecated, using nixpkgsCross.android.aarch64 instead." aarch64;
        armv7aImpure = lib.warn "nixpkgsCross.android.armv7aImpure has been deprecated, using nixpkgsCross.android.aarch32 instead." aarch32;
      };
      ios = lib.mapAttrs (_: args: nixpkgsFunc (nixpkgsArgs // args)) rec {
        simulator64 = {
          crossSystem = lib.systems.examples.iphone64-simulator // {
            sdkVer = iosSdkVersion;
          };
        };
        aarch64 = {
          crossSystem = lib.systems.examples.iphone64 // {
            sdkVer = iosSdkVersion;
          };
        };
        aarch32 = {
          crossSystem = lib.systems.examples.iphone32 // {
            sdkVer = iosSdkVersion;
          };
        };
        # Back compat
        arm64 = lib.warn "nixpkgsCross.ios.arm64 has been deprecated, using nixpkgsCross.ios.aarch64 instead." aarch64;
      };
    };

    haskellLib = nixpkgs.haskell.lib;

    filterGit = builtins.filterSource (path: type: !(builtins.any (x: x == baseNameOf path) [".git" "tags" "TAGS" "dist"]));

    # Retrieve source that is controlled by the hack-* scripts; it may be either a stub or a checked-out git repo
    hackGet = p:
      let filterArgs = x: removeAttrs x [ "branch" ];
      in if builtins.pathExists (p + "/git.json") then (
        let gitArgs = filterArgs (builtins.fromJSON (builtins.readFile (p + "/git.json")));
        in if builtins.elem "@" (lib.stringToCharacters gitArgs.url)
        then fetchgitPrivate gitArgs
        else fetchgit gitArgs)
      else if builtins.pathExists (p + "/github.json") then fetchFromGitHub (filterArgs (builtins.fromJSON (builtins.readFile (p + "/github.json"))))
      else {
        name = baseNameOf p;
        outPath = filterGit p;
      };

    # All imports of sources need to go here, so that they can be explicitly cached
    sources = {
      ghcjs8_0 = {
        boot = hackGet ./ghcjs-8.0/boot;
        shims = hackGet ./ghcjs-8.0/shims;
        ghcjs = hackGet ./ghcjs-8.0/ghcjs;
      };
    };

    optionalExtension = cond: overlay: if cond then overlay else _: _: {};

    applyPatch = patch: src: nixpkgs.runCommand "applyPatch" {
      inherit src patch;
    } ''
      cp -r "$src" "$out"

      cd "$out"
      chmod -R +w .
      patch -p1 <"$patch"
    '';

    overrideCabal = pkg: f: if pkg == null then null else haskellLib.overrideCabal pkg f;

    replaceSrc = pkg: src: version: overrideCabal pkg (drv: {
      inherit src version;
      sha256 = null;
      revision = null;
      editedCabalFile = null;
    });

    combineOverrides = old: new: old // new // lib.optionalAttrs (old ? overrides && new ? overrides) {
      overrides = lib.composeExtensions old.overrides new.overrides;
    };

    # Makes sure that old `overrides` from a previous call to `override` are not
    # forgotten, but composed. Do this by overriding `override` and passing a
    # function which takes the old argument set and combining it. What a tongue
    # twister!
    makeRecursivelyOverridable = x: x // {
      override = new: makeRecursivelyOverridable (x.override (old: (combineOverrides old new)));
    };

    foreignLibSmuggleHeaders = pkg: overrideCabal pkg (drv: {
      postInstall = ''
        cd dist/build/${pkg.pname}/${pkg.pname}-tmp
        for header in $(find . | grep '\.h'$); do
          local dest_dir=$out/include/$(dirname "$header")
          mkdir -p "$dest_dir"
          cp "$header" "$dest_dir"
        done
      '';
    });

    cabal2nixResult = src: builtins.trace "cabal2nixResult is deprecated; use ghc.haskellSrc2nix or ghc.callCabal2nix instead" (ghc.haskellSrc2nix {
      name = "for-unknown-package";
      src = "file://${src}";
      sha256 = null;
    });

    stage2Script = nixpkgs.runCommand "stage2.nix" {
      GEN_STAGE2 = builtins.readFile (nixpkgs.path + "/pkgs/development/compilers/ghcjs/gen-stage2.rb");
      buildCommand = ''
        echo "$GEN_STAGE2" > gen-stage2.rb && chmod +x gen-stage2.rb
        patchShebangs .
        ./gen-stage2.rb "${sources.ghcjs8_0.boot}" >"$out"
      '';
      nativeBuildInputs = with nixpkgs; [
        ruby cabal2nix
      ];
    } "";

    ghcjsApplyFastWeak = ghcjs: ghcjs.overrideAttrs (drv: {
      patches = (drv.patches or [])
        ++ lib.optional useFastWeak ./fast-weak.patch;
      phases = [ "unpackPhase" "patchPhase" "buildPhase" ];
    });
    useTextJSStringAsBootPkg = ghcjs: if !useTextJSString then ghcjs else ghcjs.overrideAttrs (_: {
      postUnpack = ''
        set -x
        (
          echo $sourceRoot
          cd $sourceRoot
          rm -r lib/boot/pkg/text
          cp --no-preserve=mode -r "${textSrc}" lib/boot/pkg/text
          cp --no-preserve=mode -r "${ghcjsBaseTextJSStringSrc}" lib/boot/pkg/ghcjs-base
          cp --no-preserve=mode -r "${dlistSrc}" lib/boot/pkg/dlist
          rm -r lib/boot/pkg/vector
          cp --no-preserve=mode -r "${vectorSrc}" lib/boot/pkg/vector
          sed -i 's/.\/pkg\/mtl/.\/pkg\/mtl\n    - .\/pkg\/ghcjs-base\n    - .\/pkg\/dlist\n    - .\/pkg\/primitive\n    - .\/pkg\/vector/' lib/boot/boot.yaml
          cat lib/boot/boot.yaml
        )
      '';
    });
    ghcjsBaseSrc = fetchgit {
      url = "https://github.com/ghcjs/ghcjs-base.git";
      rev = "01014ade3f8f5ae677df192d7c2a208bd795b96c";
      sha256 = "173h98m7namxj0kfy8fj29qcxmcz6ilg04x8mwkc3ydjqrvk77hh";
      postFetch = ''
        ( cd $out
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/2d0d674e54c273ed5fcb9a13f588819c3303a865.patch"; #ghcjs-base/114
            sha256 = "15vbxnxa1fpdcmmx5zx1z92bzsxyb0cbs3hs3g7fb1rkds5qbvgp";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/8eccb8d937041ba323d62dea6fe8eb1b04b3cc47.patch"; #ghcjs-base/116
            sha256 = "1lqjpg46ydpm856wcq1g7c97d69qcnnqs5jxp2b788z9cfd5n64c";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/ce91c525b5d4377ba4aefd0d8072dc1659f75ef1.patch"; #ghcjs-base/118
            sha256 = "0f6qca1i60cjzpbq4bc74baa7xrf417cja8nmhfims1fflvsx3wy";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/213bfc74a051242668edf0533e11a3fafbbb1bfe.patch"; #ghcjs-base/120
            sha256 = "0d5dwy22hxa79l8b4y6nn53nbcs74686s0rmfr5l63sdvqxhdy3x";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/82d76814ab40dc9116990f69f16df330462f27d4.patch"; #ghcjs-base/121
            sha256 = "0qa74h6w8770csad0bky4hhss1b1s86i6ccpd3ky4ljx00272gqh";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/5eb34b3dfc6fc9196931178a7a6e2c8a331a8e53.patch"; #ghcjs-base/122
            sha256 = "1wrfi0rscy8qa9pi4siv54pq5alplmy56ym1fbs8n93xwlqhddii";
          }}
          patch -p1 < ${nixpkgs.fetchpatch {
            url = "https://github.com/ghcjs/ghcjs-base/commit/0cf64df77cdd6275d86ec6276fcf947fa58e548b.patch"; #ghcjs-base/122
            sha256 = "16wdghfsrzrb1y7lscbf9aawgxi3kvbgdjwvl1ga2zzm4mq139dr";
          }}
          cat ghcjs-base.cabal
        )
      '';
    };
    ghcjsBaseTextJSStringSrc = ghcjsBaseSrc.overrideAttrs (drv: {
      outputHash = "1ggfklrmawqh54ins98rpr7qy3zbcqaqp1w7qmh90mq5jf711x9r";
      postFetch = (drv.postFetch or "") + ''
        ( cd $out
          patch -p1 < ${./haskell-overlays/text-jsstring/ghcjs-base-text-jsstring.patch}
        )
      '';
    });

    textSrc = fetchgit {
      url = "https://github.com/obsidiansystems/text.git";
      rev = "50076be0262203f0d2afdd0b190a341878a08e21";
      sha256 = "1vy7a81b1vcbfhv7l3m7p4hx365ss13mzbzkjn9751bn4n7x2ydd";
    };
    dlistSrc = fetchgit {
      url = "https://github.com/spl/dlist.git";
      rev = "03d91a3000cba49bd2c8588cf1b0d71e229ad3b0"; #v0.8.0.4
      sha256 = "0asvz1a2rp174r3vvgs1qaidxbdxly4mnlra33dipd0gxrrk15sq";
    };
    vectorSrc = fetchgit {
      url = "https://github.com/haskell/vector.git";
      rev = "1d208ee9e3a252941ebd112e14e8cd5a982ac2bb"; #v0.12.0.1
      sha256 = "18qm1c2zqr8h150917djfc0xk62hv99b1clxfs9a79aavrsqi5hs";
      postFetch = ''
        substituteInPlace $out/vector.cabal --replace 'base >= 4.5 && < 4.10' 'base >= 4.5 && < 5'
      '';
    };

  ghcSavedSplices = ghcSavedSplices-8_4;
  ghcSavedSplices-8_4 = (makeRecursivelyOverridable nixpkgs.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = lib.foldr lib.composeExtensions (_: _: {}) (let
      haskellOverlays = nixpkgs.haskell.overlays;
    in [
      haskellOverlays.combined
      haskellOverlays.saveSplices
      (self: super: with haskellLib; {
        cryptonite = disableCabalFlag super.cryptonite "integer-gmp";
        integer-logarithms = disableCabalFlag super.integer-logarithms "integer-gmp";
        scientific = enableCabalFlag super.scientific "integer-simple";
        dependent-sum-template = dontCheck super.dependent-sum-template;
        generic-deriving = dontCheck super.generic-deriving;
      })
    ]);
  };
  ghcjs = if __useLegacyCompilers then ghcjs8_0 else ghcjs8_4;
  ghcjs8_4 = (makeRecursivelyOverridable (nixpkgs.haskell.packages.ghcjs84.override (old: {
    ghc = useTextJSStringAsBootPkg (ghcjsApplyFastWeak (old.ghc.override {
      ghcjsSrc = fetchgit {
        url = "https://github.com/obsidiansystems/ghcjs.git";
        rev = "584eaa138c32c5debb3aae571c4153d537ff58f1";
        sha256 = "1ib0vsv2wrwf5iivnq6jw2l9g5izs0fjpp80jrd71qyywx4xcm66";
        fetchSubmodules = true;
      };
    }));
  }))).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghcjs8_2 = (makeRecursivelyOverridable (nixpkgs.haskell.packages.ghcjs82.override (old: {
    ghc = ghcjsApplyFastWeak (old.ghc.override {
      ghcjsDepOverrides = self: super: {
        haddock-library-ghcjs = haskellLib.doJailbreak super.haddock-library-ghcjs;
      };
    });
  }))).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghcjs8_0 = (makeRecursivelyOverridable (nixpkgs.haskell.packages.ghcjs80.override (old: {
    ghc = (import "${nixpkgs.path}/pkgs/development/compilers/ghcjs/8.0" {
      bootPkgs = nixpkgs.haskell.packages.ghc802.override {
        overrides = self: super: {
          # Newer versions no longer export `(<>)`, because that is now
          # understand to be monoid/semigroup append.
          wl-pprint-text = haskellLib.doJailbreak (self.callHackage "wl-pprint-text" "1.1.1.0" {});
          # Old `wl-pprint-text` in turn doesn't expect `base-compat` to provide
          # a `(<>)`, since it is defining its own.
          base-compat = self.callHackage "base-compat" "0.9.3" {};
          # relax bounds for newer process
          concurrent-output = haskellLib.doJailbreak super.concurrent-output;
          # missing semigroups pkg
          ListLike = haskellLib.addBuildDepend super.ListLike self.semigroups;
        };
      };
      inherit (nixpkgs) cabal-install;
      inherit (nixpkgs.buildPackages) fetchgit fetchFromGitHub;
    }).override {
      ghcjsSrc = sources.ghcjs8_0.ghcjs;
      ghcjsBootSrc = sources.ghcjs8_0.boot;
      shims = sources.ghcjs8_0.shims;
      stage2 = import stage2Script;
    };
  }))).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };

  ghc = if __useLegacyCompilers then ghc8_0 else ghc8_4;
  ghcHEAD = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghcHEAD).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghc8_4 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc843).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghc8_2 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc822).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghc8_0 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc802).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };
  ghc7 = (makeRecursivelyOverridable nixpkgs.haskell.packages.ghc7103).override {
    overrides = nixpkgs.haskell.overlays.combined;
  };

  # Takes a package set with `makeRecursivelyOverridable` and ensures that any
  # future overrides will be applied to both the package set itself and it's
  # build-time package set (`buildHaskellPackages`).
  makeRecursivelyOverridableBHPToo = x: x // {
    override = new: makeRecursivelyOverridableBHPToo (x.override
      (combineOverrides
        {
          overrides = self: super: {
            buildHaskellPackages = super.buildHaskellPackages.override new;
          };
        }
        new));
  };

  ghcAndroidAarch64 = if __useLegacyCompilers then ghcAndroidAarch64-8_2 else ghcAndroidAarch64-8_4;
  ghcAndroidAarch64-8_4 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.android.aarch64.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = nixpkgsCross.android.aarch64.haskell.overlays.combined;
  });
  ghcAndroidAarch64-8_2 = (makeRecursivelyOverridable nixpkgsCross.android.aarch64.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgsCross.android.aarch64.haskell.overlays.combined;
  };
  ghcAndroidAarch32 = if __useLegacyCompilers then ghcAndroidAarch32-8_2 else ghcAndroidAarch32-8_4;
  ghcAndroidAarch32-8_4 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.android.aarch32.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = nixpkgsCross.android.aarch32.haskell.overlays.combined;
  });
  ghcAndroidAarch32-8_2 = (makeRecursivelyOverridable nixpkgsCross.android.aarch32.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgsCross.android.aarch32.haskell.overlays.combined;
  };

  ghcIosSimulator64 = if __useLegacyCompilers then ghcIosSimulator64-8_2 else ghcIosSimulator64-8_4;
  ghcIosSimulator64-8_4 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.simulator64.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = nixpkgsCross.ios.simulator64.haskell.overlays.combined;
  });
  ghcIosSimulator64-8_2 = (makeRecursivelyOverridable nixpkgsCross.ios.simulator64.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgsCross.ios.simulator64.haskell.overlays.combined;
  };
  ghcIosAarch64 = if __useLegacyCompilers then ghcIosAarch64-8_2 else ghcIosAarch64-8_4;
  ghcIosAarch64-8_4 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.aarch64.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = nixpkgsCross.ios.aarch64.haskell.overlays.combined;
  });
  ghcIosAarch64-8_2 = (makeRecursivelyOverridable nixpkgsCross.ios.aarch64.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgsCross.ios.aarch64.haskell.overlays.combined;
  };
  ghcIosAarch32 = if __useLegacyCompilers then ghcIosAarch32-8_2 else ghcIosAarch32-8_4;
  ghcIosAarch32-8_4 = makeRecursivelyOverridableBHPToo ((makeRecursivelyOverridable nixpkgsCross.ios.aarch32.haskell.packages.integer-simple.ghcSplices-8_4).override {
    overrides = nixpkgsCross.ios.aarch32.haskell.overlays.combined;
  });
  ghcIosAarch32-8_2 = (makeRecursivelyOverridable nixpkgsCross.ios.aarch32.haskell.packages.integer-simple.ghc822).override {
    overrides = nixpkgsCross.ios.aarch32.haskell.overlays.combined;
  };

  #TODO: Separate debug and release APKs
  #TODO: Warn the user that the android app name can't include dashes
  android = androidWithHaskellPackages {
    inherit ghcAndroidAarch64 ghcAndroidAarch32;
  };
  android-8_4 = androidWithHaskellPackages {
    ghcAndroidAarch64 = ghcAndroidAarch64-8_4;
    ghcAndroidAarch32 = ghcAndroidAarch32-8_4;
  };
  android-8_2 = androidWithHaskellPackages {
    ghcAndroidAarch64 = ghcAndroidAarch64-8_2;
    ghcAndroidAarch32 = ghcAndroidAarch32-8_2;
  };
  androidWithHaskellPackages = { ghcAndroidAarch64, ghcAndroidAarch32 }: import ./android {
    inherit nixpkgs nixpkgsCross ghcAndroidAarch64 ghcAndroidAarch32 overrideCabal;
  };
  iosAarch64 = iosWithHaskellPackages ghcIosAarch64;
  iosAarch64-8_4 = iosWithHaskellPackages ghcIosAarch64-8_4;
  iosAarch64-8_2 = iosWithHaskellPackages ghcIosAarch64-8_2;
  iosAarch32 = iosWithHaskellPackages ghcIosAarch32;
  iosAarch32-8_4 = iosWithHaskellPackages ghcIosAarch32-8_4;
  iosAarch32-8_2 = iosWithHaskellPackages ghcIosAarch32-8_2;
  iosWithHaskellPackages = ghc: {
    buildApp = import ./ios { inherit nixpkgs ghc; };
  };

in let this = rec {
  inherit nixpkgs
          nixpkgsCross
          overrideCabal
          hackGet
          foreignLibSmuggleHeaders
          stage2Script
          ghc
          ghcHEAD
          ghc8_4
          ghc8_2
          ghc8_0
          ghc7
          ghcIosSimulator64
          ghcIosAarch64
          ghcIosAarch64-8_4
          ghcIosAarch64-8_2
          ghcIosAarch32
          ghcIosAarch32-8_4
          ghcIosAarch32-8_2
          ghcAndroidAarch64
          ghcAndroidAarch64-8_4
          ghcAndroidAarch64-8_2
          ghcAndroidAarch32
          ghcAndroidAarch32-8_4
          ghcAndroidAarch32-8_2
          ghcjs
          ghcjs8_0
          ghcjs8_2
          ghcjs8_4
          ghcSavedSplices
          android
          androidWithHaskellPackages
          iosAarch32
          iosAarch64
          iosWithHaskellPackages
          filterGit;

  # Back compat
  ios = iosAarch64;
  ghcAndroidArm64 = lib.warn "ghcAndroidArm64 has been deprecated, using ghcAndroidAarch64 instead." ghcAndroidAarch64;
  ghcAndroidArmv7a = lib.warn "ghcAndroidArmv7a has been deprecated, using ghcAndroidAarch32 instead." ghcAndroidAarch32;
  ghcIosArm64 = lib.warn "ghcIosArm64 has been deprecated, using ghcIosAarch64 instead." ghcIosAarch64;

  androidReflexTodomvc = android.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc";
    displayName = "Reflex TodoMVC";
  };
  androidReflexTodomvc-8_4 = android-8_4.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc.via_8_4";
    displayName = "Reflex TodoMVC via GHC 8.4";
  };
  androidReflexTodomvc-8_2 = android-8_2.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    applicationId = "org.reflexfrp.todomvc.via_8_2";
    displayName = "Reflex TodoMVC via GHC 8.2";
  };
  iosReflexTodomvc = ios.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc";
    bundleName = "Reflex TodoMVC";
  };
  iosReflexTodomvc-8_4 = iosAarch64-8_4.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc.via_8_4";
    bundleName = "Reflex TodoMVC via GHC 8.4";
  };
  iosReflexTodomvc-8_2 = iosAarch64-8_2.buildApp {
    package = p: p.reflex-todomvc;
    executableName = "reflex-todomvc";
    bundleIdentifier = "org.reflexfrp.todomvc.via_8_2";
    bundleName = "Reflex TodoMVC via GHC 8.2";
  };
  setGhcLibdir = ghcLibdir: inputGhcjs:
    let libDir = "$out/lib/ghcjs-${inputGhcjs.version}";
        ghcLibdirLink = nixpkgs.stdenv.mkDerivation {
          name = "ghc_libdir";
          inherit ghcLibdir;
          buildCommand = ''
            mkdir -p ${libDir}
            echo "$ghcLibdir" > ${libDir}/ghc_libdir_override
          '';
        };
    in inputGhcjs // {
    outPath = nixpkgs.buildEnv {
      inherit (inputGhcjs) name;
      paths = [ inputGhcjs ghcLibdirLink ];
      postBuild = ''
        mv ${libDir}/ghc_libdir_override ${libDir}/ghc_libdir
      '';
    };
  };

  platforms = [
    "ghcjs"
    "ghc"
  ];

  attrsToList = s: map (name: { inherit name; value = builtins.getAttr name s; }) (builtins.attrNames s);
  mapSet = f: s: builtins.listToAttrs (map ({name, value}: {
    inherit name;
    value = f value;
  }) (attrsToList s));
  mkSdist = pkg: pkg.override (oldArgs: {
    mkDerivation = drv: oldArgs.mkDerivation (drv // {
      postConfigure = ''
        ./Setup sdist
        mkdir "$out"
        mv dist/*.tar.gz "$out/${drv.pname}-${drv.version}.tar.gz"
        exit 0
      '';
      doHaddock = false;
    });
  });
  sdists = mapSet mkSdist ghc;
  mkHackageDocs = pkg: pkg.override (oldArgs: {
    mkDerivation = drv: oldArgs.mkDerivation (drv // {
      postConfigure = ''
        ./Setup haddock --hoogle --hyperlink-source --html --for-hackage --haddock-option=--built-in-themes
        cd dist/doc/html
        mkdir "$out"
        tar cz --format=ustar -f "$out/${drv.pname}-${drv.version}-docs.tar.gz" "${drv.pname}-${drv.version}-docs"
        exit 0
      '';
      doHaddock = false;
    });
  });
  hackageDocs = mapSet mkHackageDocs ghc;
  mkReleaseCandidate = pkg: nixpkgs.stdenv.mkDerivation (rec {
    name = pkg.name + "-rc";
    sdist = mkSdist pkg + "/${pkg.pname}-${pkg.version}.tar.gz";
    docs = mkHackageDocs pkg + "/${pkg.pname}-${pkg.version}-docs.tar.gz";

    builder = builtins.toFile "builder.sh" ''
      source $stdenv/setup

      mkdir "$out"
      echo -n "${pkg.pname}-${pkg.version}" >"$out/pkgname"
      ln -s "$sdist" "$docs" "$out"
    '';

    # 'checked' isn't used, but it is here so that the build will fail
    # if tests fail
    checked = overrideCabal pkg (drv: {
      doCheck = true;
      src = sdist;
    });
  });
  releaseCandidates = mapSet mkReleaseCandidate ghc;

  androidDevTools = [
    ghc.haven
    nixpkgs.maven
    nixpkgs.androidsdk
  ];

  # Tools that are useful for development under both ghc and ghcjs
  generalDevToolsAttrs = haskellPackages:
    let nativeHaskellPackages = ghc;
    in {
    inherit (nativeHaskellPackages)
      Cabal
      cabal-install
      ghcid
      hasktags
      hlint;
    inherit (nixpkgs)
      cabal2nix
      curl
      nix-prefetch-scripts
      nodejs
      pkgconfig
      closurecompiler;
  } // (lib.optionalAttrs (!(haskellPackages.ghc.isGhcjs or false) && builtins.compareVersions haskellPackages.ghc.version "8.2" < 0) {
    # ghc-mod doesn't currently work on ghc 8.2.2; revisit when https://github.com/DanielG/ghc-mod/pull/911 is closed
    # When ghc-mod is included in the environment without being wrapped in justStaticExecutables, it prevents ghc-pkg from seeing the libraries we install
    ghc-mod = (nixpkgs.haskell.lib.justStaticExecutables haskellPackages.ghc-mod);
    inherit (haskellPackages) hdevtools;
  }) // (lib.optionalAttrs (builtins.compareVersions haskellPackages.ghc.version "7.10" >= 0) {
    inherit (nativeHaskellPackages) stylish-haskell; # Recent stylish-haskell only builds with AMP in place
  });

  generalDevTools = haskellPackages: builtins.attrValues (generalDevToolsAttrs haskellPackages);

  nativeHaskellPackages = haskellPackages:
    if haskellPackages.isGhcjs or false
    then haskellPackages.ghc
    else haskellPackages;

  workOn = haskellPackages: package: (overrideCabal package (drv: {
    buildDepends = (drv.buildDepends or []) ++ generalDevTools (nativeHaskellPackages haskellPackages);
  })).env;

  workOnMulti' = { env, packageNames, tools ? _: [], shellToolOverrides ? _: _: {} }:
    let inherit (builtins) listToAttrs filter attrValues all concatLists;
        combinableAttrs = [
          "benchmarkDepends"
          "benchmarkFrameworkDepends"
          "benchmarkHaskellDepends"
          "benchmarkPkgconfigDepends"
          "benchmarkSystemDepends"
          "benchmarkToolDepends"
          "buildDepends"
          "buildTools"
          "executableFrameworkDepends"
          "executableHaskellDepends"
          "executablePkgconfigDepends"
          "executableSystemDepends"
          "executableToolDepends"
          "extraLibraries"
          "libraryFrameworkDepends"
          "libraryHaskellDepends"
          "libraryPkgconfigDepends"
          "librarySystemDepends"
          "libraryToolDepends"
          "pkgconfigDepends"
          "setupHaskellDepends"
          "testDepends"
          "testFrameworkDepends"
          "testHaskellDepends"
          "testPkgconfigDepends"
          "testSystemDepends"
          "testToolDepends"
        ];
        concatCombinableAttrs = haskellConfigs: lib.filterAttrs (n: v: v != []) (lib.listToAttrs (map (name: { inherit name; value = concatLists (map (haskellConfig: haskellConfig.${name} or []) haskellConfigs); }) combinableAttrs));
        getHaskellConfig = p: (overrideCabal p (args: {
          passthru = (args.passthru or {}) // {
            out = args;
          };
        })).out;
        notInTargetPackageSet = p: all (pname: (p.pname or "") != pname) packageNames;
        baseTools = generalDevToolsAttrs env;
        overriddenTools = attrValues (baseTools // shellToolOverrides env baseTools);
        depAttrs = lib.mapAttrs (_: v: filter notInTargetPackageSet v) (concatCombinableAttrs (concatLists [
          (map getHaskellConfig (lib.attrVals packageNames env))
          [{
            buildTools = overriddenTools ++ tools env;
          }]
        ]));

    in (env.mkDerivation (depAttrs // {
      pname = "work-on-multi--combined-pkg";
      version = "0";
      license = null;
    })).env;

  workOnMulti = env: packageNames: workOnMulti' { inherit env packageNames; };

  # A simple derivation that just creates a file with the names of all
  # of its inputs. If built, it will have a runtime dependency on all
  # of the given build inputs.
  pinBuildInputs = name: buildInputs: otherDeps: (nixpkgs.releaseTools.aggregate {
    inherit name;
    constituents = buildInputs ++ otherDeps;
  }).overrideAttrs (old: {
    buildCommand = old.buildCommand + ''
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs $otherDeps" > "$out/deps"
    '';
    inherit buildInputs otherDeps;
  });

  # The systems that we want to build for on the current system
  cacheTargetSystems = lib.warn "cacheTargetSystems has been deprecated, use cacheBuildSystems" cacheBuildSystems;
  cacheBuildSystems = [
    "x86_64-linux"
    # "i686-linux"
    "x86_64-darwin"
  ];

  isSuffixOf = suffix: s:
    let suffixLen = builtins.stringLength suffix;
    in builtins.substring (builtins.stringLength s - suffixLen) suffixLen s == suffix;

  reflexEnv = platform:
    let haskellPackages = builtins.getAttr platform this;
        ghcWithStuff = if platform == "ghc" || platform == "ghcjs"
                       then haskellPackages.ghcWithHoogle
                       else haskellPackages.ghcWithPackages;
    in ghcWithStuff (p: import ./packages.nix {
      haskellPackages = p;
      inherit platform;
    });

  tryReflexPackages = generalDevTools ghc
    ++ builtins.map reflexEnv platforms;

  cachePackages =
    let otherPlatforms = lib.optionals androidSupport [
          "ghcAndroidAarch64"
          "ghcAndroidAarch32"
        ] ++ lib.optional iosSupport "ghcIosAarch64";
    in tryReflexPackages
      ++ builtins.map reflexEnv otherPlatforms
      ++ lib.optionals androidSupport [
        androidDevTools
        androidReflexTodomvc
      ] ++ lib.optionals iosSupport [
        iosReflexTodomvc
      ];

  demoVM = (import "${nixpkgs.path}/nixos" {
    configuration = {
      imports = [
        "${nixpkgs.path}/nixos/modules/virtualisation/virtualbox-image.nix"
        "${nixpkgs.path}/nixos/modules/profiles/demo.nix"
      ];
      environment.systemPackages = tryReflexPackages;
      nixpkgs = { localSystem.system = "x86_64-linux"; };
    };
  }).config.system.build.virtualBoxOVA;

  inherit cabal2nixResult sources system androidSupport iosSupport;
  project = args: import ./project this (args ({ pkgs = nixpkgs; } // this));
  tryReflexShell = pinBuildInputs ("shell-" + system) tryReflexPackages [];
  js-framework-benchmark-src = hackGet ./js-framework-benchmark;
  ghcjsExternsJs = ./ghcjs.externs.js;
}; in this

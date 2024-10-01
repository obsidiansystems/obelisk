{ system ? builtins.currentSystem
, profiling ? false
, iosSdkVersion ? "15.0"
, config ? {}
, terms ? { # Accepted terms, conditions, and licenses
    security.acme.acceptTerms = false;
  }
, useGHC810 ? true # false if one wants to use ghc 8.6.5
}:
let
  inherit (import dep/gitignore.nix { inherit (pkgs) lib; }) gitignoreSource;

  nix-thunk = import ./dep/nix-thunk {};
  mars = nix-thunk.thunkSource ./dep/mars;
  marsProject = args: (import mars args).project;

  obeliskHackageOverlays = [
    {
      name = "obelisk-asset-serve-snap";
      version = "0.1";
      src = ./lib/asset/serve-snap;
    }
    {
      name = "obelisk-asset-manifest";
      version = "0.1";
      src = ./lib/asset/manifest;
    }
    {
      name = "obelisk-backend";
      version = "0.1";
      src = ./lib/backend;
    }
    {
      name = "obelisk-command";
      version = "0.1";
      src = ./lib/command;
    }
    {
      name = "obelisk-executable-config-inject";
      version = "0.1";
      src = ./lib/executable-config/inject;
    }
    {
      name = "obelisk-executable-config-lookup";
      version = "0.1";
      src = ./lib/executable-config/lookup;
    }
    {
      name = "obelisk-frontend";
      version = "0.1";
      src = ./lib/frontend;
    }
    {
      name = "obelisk-route";
      version = "0.1";
      src = ./lib/route;
    }
    {
      name = "obelisk-run";
      version = "0.1";
      src = ./lib/run;
    }
    {
      name = "obelisk-selftest";
      version = "0.1";
      src = ./lib/selftest;
    }
    {
      name = "obelisk-snap-extras";
      version = "0.1";
      src = ./lib/snap-extras;
    }
    {
      name = "tabulation";
      version = "0.1.0.0";
      src = ./lib/tabulation;
    }
  ];

  obeliskProjDef = { enableLibraryProfiling ? false }: ({ pkgs, thunkSource, ... }: {
    name = "obelisk-pkgs";
    src = ./.;
    index-state = "2023-03-04T00:00:00Z";
    extraArgs = {
      staticFiles = [ null ];
    };
    shellTools = {
      cabal = "3.2.0.0";
      # haskell-language-server = "1.5.0.0";
    };
    shells = ps: with ps; [
      obelisk-command
      obelisk-backend
      obelisk-asset-manifest
      obelisk-asset-serve-snap
      obelisk-executable-config-inject
      obelisk-executable-config-lookup
      obelisk-frontend
      obelisk-route
      obelisk-run
      obelisk-selftest
    ];
    overrides = [
      ({ config, pkgs, lib, ... }: { packages.git.src = thunkSource ./dep/git; })
      ({ config, pkgs, lib, ... }: {
        packages.cli-git.components.library.build-tools = [
          pkgs.git
        ];
        #packages.nix-thunk.src = thunkSource ./dep/nix-thunk;
        packages.obelisk-command.components.library.build-tools = [
          config.hsPkgs.ghcid.components.exes.ghcid
          pkgs.nixVersions.nix_2_12
          pkgs.nix-prefetch-git
          pkgs.openssh
          pkgs.rsync
          pkgs.which
          pkgs.jre
          pkgs.git
        ];
        packages.obelisk-run.components.library.build-tools = with pkgs; [
          iproute
        ];
        packages.obelisk-selftest.components.library.build-tools = with pkgs; [
          cabal-install
          git
          nixVersions.nix_2_12
          nix-prefetch-git
          rsync
          which
          jre
          openssh
        ];
        packages.nix-thunk.components.library.build-tools = [
          (pkgs.writeTextFile {
            name = "print-nixpkgs-path";
            text = ''
              #!/bin/sh
              echo "${nix-thunk.packedThunkNixpkgs}"
            '';
            executable = true;
            destination = "/bin/print-nixpkgs-path";
          })
        ];
      })
    ] ++ pkgs.lib.optionals (enableLibraryProfiling) [
      ({ pkgs, config, lib, ... }: {
        config.enableLibraryProfiling = true;
      })
    ];
  });
  marsObelisk = marsProject {} (obeliskProjDef {});

  pkgs = marsObelisk.pkgs;
  nixpkgs = marsObelisk.pkgs;

  inherit (import ./lib/asset/assets.nix { inherit nixpkgs; }) mkAssets;

  haskellLib = pkgs.haskell.lib;
in rec {
  inherit marsObelisk;
  inherit (nixpkgs) lib;
  inherit nixpkgs pkgs;
  pathGit = ./.;  # Used in CI by the migration graph hash algorithm to correctly ignore files.
  command = marsObelisk.hsPkgs.obelisk-command.components.exes.ob;
  shell = nixpkgs.mkShell {
    name = "obelisk";
    buildInputs = [
      command
    ];
  };


  selftest = pkgs.writeScript "selftest" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail

    PATH="${command}/bin:$PATH"
    cd ${./.}
    "${marsObelisk.hsPkgs.obelisk-selftest.components.obelisk-selftest}/bin/obelisk-selftest" +RTS -N -RTS "$@"
  '';
  skeleton = pkgs.runCommand "skeleton" {
    dir = builtins.filterSource (path: type: builtins.trace path (baseNameOf path != ".obelisk")) ./skeleton;
  } ''
    ln -s "$dir" "$out"
  '';
  nullIfAbsent = p: if lib.pathExists p then p else null;
  #TODO: Avoid copying files within the nix store.  Right now, obelisk-asset-manifest-generate copies files into a big blob so that the android/ios static assets can be imported from there; instead, we should get everything lined up right before turning it into an APK, so that copies, if necessary, only exist temporarily.
  processAssets = { src,
                    packageName ? "obelisk-generated-static",
                    moduleName ? "Obelisk.Generated.Static",
                    exe ? "obelisk-asset-th-generate",
                    obeliskPkgs ? marsObelisk.hsPkgs
                  }:
  pkgs.runCommand "asset-manifest" {
    inherit src;
    outputs = [ "out" "haskellManifest" "symlinked" ];
    nativeBuildInputs = [ obeliskPkgs.obelisk-asset-manifest.components.exes.obelisk-asset-th-generate ];
  } ''
    set -euo pipefail
    touch "$out"
    mkdir -p "$symlinked"
    ${exe} "$src" "$haskellManifest" ${packageName} ${moduleName} "$symlinked"
  '';

  compressedJs = frontend: optimizationLevel: externs: pkgs.runCommand "compressedJs" {} ''
    set -euo pipefail
    cd '${frontend}'
    shopt -s globstar
    for f in **/all.js; do
      dir="$out/$(basename "$(dirname "$f")")"
      mkdir -p "$dir"
      ln -s "$(realpath "$f")" "$dir/all.unminified.js"
      ${if optimizationLevel == null then ''
        ln -s "$dir/all.unminified.js" "$dir/all.js"
      '' else ''
        # NOTE: "--error_format JSON" avoids closurecompiler crashes when trying to report errors.
        '${pkgs.closurecompiler}/bin/closure-compiler' --error_format JSON ${if externs == null then "" else "--externs '${externs}'"} --externs '${mars.ghcjsExternsJs}' -O '${optimizationLevel}' --jscomp_warning=checkVars --warning_level=QUIET --create_source_map="$dir/all.js.map" --source_map_format=V3 --js_output_file="$dir/all.js" "$dir/all.unminified.js"
        echo '//# sourceMappingURL=all.js.map' >> "$dir/all.js"
      ''}
    done
  '';

  serverModules = {
    mkBaseEc2 = { nixosPkgs, ... }: {...}: {
      imports = [
        (nixosPkgs.path + /nixos/modules/virtualisation/amazon-image.nix)
      ];
      ec2.hvm = true;
    };

    mkDefaultNetworking = { adminEmail, enableHttps, hostName, routeHost, redirectHosts, ... }: {...}: {
      networking = {
        inherit hostName;
        firewall.allowedTCPPorts = if enableHttps then [ 80 443 ] else [ 80 ];
      };

      # `amazon-image.nix` already sets these but if the user provides their own module then
      # forgetting these can cause them to lose access to the server!
      # https://github.com/NixOS/nixpkgs/blob/fab05f17d15e4e125def4fd4e708d205b41d8d74/nixos/modules/virtualisation/amazon-image.nix#L133-L136
      services.openssh.enable = true;
      services.openssh.permitRootLogin = "prohibit-password";

      security.acme = if enableHttps then {
        acceptTerms = terms.security.acme.acceptTerms;
        email = adminEmail;
        certs = {
          "${routeHost}" = {
            extraDomains = builtins.listToAttrs (map (h: { name = h; value = null; }) redirectHosts);
          };
        };
      } else {};
    };

    mkObeliskApp =
      { exe
      , routeHost
      , enableHttps
      , name ? "backend"
      , user ? name
      , group ? user
      , baseUrl ? "/"
      , internalPort ? 8000
      , backendArgs ? "--port=${toString internalPort}"
      , redirectHosts ? [] # Domains to redirect to routeHost; importantly, these domains will be added to the SSL certificate
      , configHash ? "" # The expected hash of the configuration directory tree.
      , ...
      }: {...}:
      assert lib.assertMsg (!(builtins.elem routeHost redirectHosts)) "routeHost may not be a member of redirectHosts";
      {
      services.nginx = {
        enable = true;
        recommendedProxySettings = true;
        virtualHosts = {
          "${routeHost}" = {
            enableACME = enableHttps;
            forceSSL = enableHttps;
            locations.${baseUrl} = {
              proxyPass = "http://127.0.0.1:" + toString internalPort;
              proxyWebsockets = true;
              extraConfig = ''
                access_log off;
              '';
            };
          };
        } // builtins.listToAttrs (map (redirectSourceDomain: {
          name = redirectSourceDomain;
          value = {
            enableACME = enableHttps;
            forceSSL = enableHttps;
            globalRedirect = routeHost;
          };
        }) redirectHosts);
      };
      systemd.services.${name} = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        restartIfChanged = true;
        path = [ pkgs.gnutar ];

        # Even though echoing the hash is functionally useless at
        # runtime, its inclusion in the service script means that Nix
        # will automatically restart the server whenever the configHash
        # argument is changed.
        script = ''
          echo "Expecting config hash to be ${configHash}, but not verifying this"
          ln -sft . '${exe}'/*
          mkdir -p log
          exec ./backend ${backendArgs} </dev/null
        '';

        serviceConfig = {
          User = user;
          KillMode = "process";
          WorkingDirectory = "~";
          Restart = "always";
          RestartSec = 5;
        };
      };
      users = {
        users.${user} = {
          description = "${user} service";
          home = "/var/lib/${user}";
          createHome = true;
          isSystemUser = true;
          group = group;
        };
        groups.${group} = {};
      };
    };
  };

  inherit mkAssets;

  serverExe = backend: frontend: assets: optimizationLevel: externjs: version:
    let
      exeBackend = backend;
      exeFrontend = compressedJs frontend optimizationLevel externjs;
      exeFrontendAssets = mkAssets exeFrontend;
      exeAssets = mkAssets assets;
    in pkgs.runCommand "serverExe" {
          backend = exeBackend.outPath;
          frontend = exeFrontend.outPath;
          frontendAssets = exeFrontendAssets.outPath;
          staticAssets = exeAssets.outPath;
    } ''
      mkdir $out
      set -eux
      ln -s ${exeBackend}/bin/backend $out/
      ln -s ${exeBackend}/bin/.backend-wrapped $out/
      ln -s $staticAssets $out/static.assets
      for d in $frontendAssets/*; do
        ln -s "$d" "$out"/"$(basename "$d").assets"
      done
      echo ${version} > $out/version
    '';

  serverModule = { exe, hostName, adminEmail, routeHost, enableHttps, version, redirectHosts ? [], configHash ? "", ... }@args: {...}: {
    imports = [
      ((args.module or (serverModules.mkBaseEc2)) { inherit (args) exe hostName adminEmail routeHost enableHttps version; nixosPkgs = pkgs; })
      (serverModules.mkDefaultNetworking args)
      (serverModules.mkObeliskApp args)
    ];
  };

  serverBuild = args:
    let
      nixos = import (pkgs.path + /nixos);
    in nixos {
      system = "x86_64-linux";
      configuration = {
        imports = [(serverModule args)];
      };
    };

  project = args: projectDef: let
    pDef = ({ pkgs, thunkSource, ... }: let
      userDef = (projectDef { inherit pkgs thunkSource; });
    in userDef // {
        extraArgs = {
          extraCabalProject = [ "optional-packages: ${./lib/run}" ];
        } // userDef.extraArgs;
      });

    proj' = (marsProject args pDef).extend (self: super: let
      reflexHasAttr = b: if super.helpers.bot_args ? b then b else null;
      reflexHasAttrBool = b: if super.helpers.bot_args ? b then true else false;
      reflexHasAttrExtra = b: if self.helpers.bot_args.extraArgs ? b then b else null;
      checkForStaticFiles = a: b: if self.userSettings.staticFiles == null then a else b;
    in rec {
        overrideProject = f: project args (defArgs:
          let def = pDef defArgs;
          in def // f def
        );

        inherit pDef;
        inherit marsObelisk;
        packageNames = {
          frontendName = "frontend";
          backendName = "backend";
          commonName = "common";
          staticName = "obelisk-generated-static";
        };

        deployLinuxServerModule = {version, buildConfigs, redirectHosts ? [], configHash ? ""}: serverModule ({
          inherit version redirectHosts configHash;
          exe = exe;
          #version;
        } // buildConfigs);

        processedStatic = processAssets {
          src = self.userSettings.staticFiles;
          exe = if lib.attrByPath ["userSettings" "__deprecated" "useObeliskAssetManifestGenerate"] false self
            then builtins.trace "obelisk-asset-manifest-generate is deprecated. Use obelisk-asset-th-generate instead." "obelisk-asset-manifest-generate"
            else "obelisk-asset-th-generate";
          obeliskPkgs = marsObelisk.hsPkgs;
        };

        extraOverlays = (checkForStaticFiles [] [
          {
            name = "obelisk-generated-static";
            version = "0";
            src = processedStatic.haskellManifest;
          }
        ]) ++ obeliskHackageOverlays;

        extraPkgDef = [
          #(hackage: (import "${marsObelisk.plan-nix}/default.nix").extras hackage)
          #(hackage: (import "${marsObelisk.plan-nix}/default.nix").pkgs hackage)
        ];

        ghcjs-app = self.crossSystems.ghcjs.hsPkgs.frontend.components.exes.frontend;

        userSettings = {
          android = reflexHasAttr "android";
          ios = reflexHasAttr "ios";
          overrides = reflexHasAttr "overrides";
          shellTools = reflexHasAttr "shellTools";
          staticFiles = if builtins.pathExists (super.args.src + "/static") then super.helpers.bot_args.extraArgs.staticFiles or (super.args.src + "/static") else null;
        };

        __androidWithConfig = configPath: {
          ${if self.userSettings.android == null then null else self.frontendName} = {
            executableName = packageNames.frontendName;
            ${if builtins.pathExists self.userSettings.staticFiles then "assets" else null} =
              nixpkgs.obeliskExecutableConfig.platforms.android.inject
                (self.injectableConfig configPath)
                self.processedStatic.symlinked;
            } // self.userSettings.android;
        };

        __iosWithConfig = configPath: {
            ${if self.userSettings.ios == null then null else self.frontendName} = {
              executableName = packageNames.frontendName;
              ${if builtins.pathExists self.userSettings.staticFiles then "staticSrc" else null} =
                nixpkgs.obeliskExecutableConfig.platforms.ios.inject
                  (self.injectableConfig configPath)
                  self.processedStatic.symlinked;
            } // self.userSettings.ios;
        };

        #combinedShell = self.shells.ghc;
        combinedShell =
          let packagesToInclude = ["backend" "common" "frontend"];
          in self.shellFor {
            withHoogle = false;
            tools = {
              cabal = "3.2.0.0";
            };
            packages = ps: builtins.map (name: ps.${name}) packagesToInclude;
            additional = ps: builtins.attrValues (builtins.removeAttrs ps packagesToInclude);
          };

        hoogleShell = self.shellFor {
          withHoogle = true;
          packages = ps: with ps; [
            common
          ];
        };

        obelisk = import (self.helpers.src + "/.obelisk/impl") { };
        exe = let
          backend = self.hsPkgs.backend.components.exes.backend;
          frontendExe = self.crossSystems.ghcjs.hsPkgs.frontend.components.exes.frontend;
          staticFiles = self.helpers.bot_args.extraArgs.staticFiles;
          ccLevel = "ADVANCED";
          externjs = null;
          version = "no_version";
        in serverExe backend frontendExe staticFiles ccLevel externjs version;

        serverOn = version:
          let backend = self.hsPkgs.backend.components.exes.backend;
              frontend = self.crossSystems.ghcjs.hsPkgs.frontend.components.exes.frontend;
              staticFiles = self.helpers.bot_args.extraArgs.staticFiles;
              ccOptLevel = "ADVANCED";
              externJs = null;
          in serverExe backend frontend staticFiles ccOptLevel externJs version;


        linuxServerModule = args@{ hostName, adminEmail, routeHost, enableHttps, version, redirectHosts ? [], configHash ? "", ...}:
          serverModule ({ module = serverModules.mkBaseEc2; exe = serverOn version; } // args);
          # the "classic flavor", as a full nixos configuration
        server = args@{ hostName, adminEmail, routeHost, enableHttps, version, module ? serverModules.mkBaseEc2, redirectHosts ? [], configHash ? "" }:
          serverBuild (args // { exe = serverOn version; });

        passthru = rec {
          staticFilesImpure = let fs = self.userSettings.staticFiles; in if lib.isDerivation fs then fs else toString fs;
          inherit processedStatic;
          profiledObelisk = marsProject {} (obeliskProjDef { enableLibraryProfiling = true; });
          obRunProfileSrc = builtins.toFile "ob-run.hs" ''
            {-# LANGUAGE NoImplicitPrelude #-}
            {-# LANGUAGE PackageImports #-}
            module Main where

            -- Explicitly import Prelude from base lest there be multiple modules called Prelude
            import "base" Prelude (Maybe(Nothing), IO, (++), read)

            import "base" Control.Exception (finally)
            import "reflex" Reflex.Profiled (writeProfilingData)
            import "base" System.Environment (getArgs)

            import qualified "obelisk-run" Obelisk.Run
            import qualified Frontend
            import qualified Backend

            main :: IO ()
            main = do
              [portStr, assets, profFileName] <- getArgs
              Obelisk.Run.run (Obelisk.Run.defaultRunApp Backend.backend Frontend.frontend (Obelisk.Run.runServeAsset assets)){ Obelisk.Run._runApp_backendPort = read portStr }
                `finally` writeProfilingData (profFileName ++ ".rprof")
          '';
          profiledObRun = self.pkgs.runCommand "ob-run" rec {
            shell = (self.shellFor { withHoogle = false; packages = ps: with ps; [ backend ]; });
            ghc = "${shell}/bin/ghc";
          } ''
            cp ${obRunProfileSrc} ob-run.hs
            mkdir -p $out/bin
            $ghc -x hs -prof -fno-prof-auto -threaded ob-run.hs -o $out/bin/ob-run
          '';
        };

        __unstable__.profiledObRun = passthru.profiledObRun;
    });
  in proj';

  test_project = project {} obeliskProjDef;
  haskellPackageSets = {
    ghc = marsObelisk.hsPkgs;
    ghcjs = marsObelisk.crossSystems.ghcjs.hsPkgs;
  };
}

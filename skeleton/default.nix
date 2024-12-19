{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;

    # In order to use Let's Encrypt for HTTPS deployments you must accept
    # their terms of service at https://letsencrypt.org/repository/.
    # Uncomment and set this to `true` to indicate your acceptance:
    # terms.security.acme.acceptTerms = false;
  }
}:
with obelisk;
let args = {
  # You must accept the Android Software Development Kit License Agreement at
  # https://developer.android.com/studio/terms in order to build Android apps.
  # Uncomment this to indicate your acceptance:
  android_sdk_accept_license = true;
  allowUnfree = true;
}; in

project args ({ pkgs, ... }: {
  name = "skeleton";
  android = {
    executable = ps: exes: (exes (ps.frontend)).frontend;
    executableName = "obelisk-skeleton";
    applicationId = "systems.obsidian.obelisk.examples.minimal";
    displayName = "Obelisk Minimal Example";
  };
  ios = {
    executable = ps: exes: (exes (ps.frontend)).frontend;
    executableName = "obelisk-skeleton";
    bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
    bundleName = "Obelisk Minimal Example";
  };
  web = {
    executable = ps: exes: (exes (ps.frontend)).frontend;
  };
  extraArgs = {
    staticFiles = ./static;
  };
  src = ./.;
  inputThunks = pkgs.obsidianCompilers.thunkSets.common ++ [
    pkgs._dep.source.aeson-1541
    pkgs._dep.source.android-activity
  ];
  shells = ps: with ps; [
    backend
  ];
  overrides = [
    ({ config, pkgs, lib, ... }: {
      config.enableShared = if pkgs.stdenv.targetPlatform.isiOS then lib.mkForce false else true;
      config.enableStatic = lib.mkForce true;
    })
    ({pkgs, lib, config, ... }: {
      packages.obelisk-run.components.library.build-tools = with pkgs; [
        iproute
      ];

      packages.reflex-dom = {
	      flags = {
	        webkit2gtk = if (pkgs.stdenv.targetPlatform.isAndroid) then lib.mkForce false else true;
	      };
      };

      packages.frontend.components.exes.frontend = {
        frameworks = if (!pkgs.stdenv.targetPlatform.isiOS && pkgs.stdenv.targetPlatform.isDarwin) then [ pkgs.darwin.apple_sdk.frameworks.CoreFoundation ] else [ ];
        postInstall = lib.optionalString (pkgs.stdenv.hostPlatform.isDarwin) ''
          mkdir -p $out/obelisk-skeleton.app
          cp -r obelisk-skeleton $out
          cp $out/bin/obelisk-skeleton $out/obelisk-skeleton.app
        '';
      };
      packages.jsaddle-wkwebview.src = (thunkSource ../dep/jsaddle) + "/jsaddle-wkwebview";
      packages.jsaddle-wkwebview.components.library = {
        frameworks =
          if (pkgs.stdenv.targetPlatform.isiOS) then lib.mkForce [ pkgs.darwin.iosSdkPkgs.sdk pkgs.darwin.apple_sdk.frameworks.CoreFoundation ]
          else [ pkgs.darwin.apple_sdk.frameworks.CoreFoundation ];
      };
    })
  ];
})

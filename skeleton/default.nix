{ system ? builtins.currentSystem
, obelisk ? import ./.obelisk/impl {
    inherit system;
    iosSdkVersion = "16.1";

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
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";

  name = "skeleton";
  extraArgs = {
    staticFiles = ./static;
  };
  src = ./.;
  inputThunks = pkgs.obsidianCompilers.thunkSets.common ++ [
    pkgs._dep.source.aeson-1541
  ];
  shells = ps: with ps; [
    backend
  ];
  overrides = [
    ({pkgs, lib, config, ... }: {
        packages.obelisk-run.components.library.build-tools = with pkgs; [
          iproute
        ];
    })
  ];
})

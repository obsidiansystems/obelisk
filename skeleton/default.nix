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
project {} ({ ... }: {
  name = "skeleton";
  extraArgs = {
    staticFiles = ./static;
  };
  src = ./.;
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

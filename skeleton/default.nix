{ obelisk ? import ./.obelisk/impl {
    system = builtins.currentSystem;
    iosSdkVersion = "10.2";
    # You must accept the Android Software Development Kit License Agreement at
    # https://developer.android.com/studio/terms in order to build Android apps.
    # Uncomment and set this to `true` to indicate your acceptance:
    # config.android_sdk.accept_license = false;
  }
}:
with obelisk;
project ./. ({ pkgs, hackGet, ... }: {
  android.applicationId = "systems.obsidian.obelisk.examples.minimal";
  android.displayName = "Obelisk Minimal Example";
  ios.bundleIdentifier = "systems.obsidian.obelisk.examples.minimal";
  ios.bundleName = "Obelisk Minimal Example";

  packages = {
    reflex-dom-core = (hackGet ./dep/reflex-dom) + /reflex-dom-core;
    reflex-dom = (hackGet ./dep/reflex-dom) + /reflex-dom;
  };

  overrides = self: super: {
    ghcjs-dom = pkgs.haskell.lib.overrideCabal (super.ghcjs-dom) (drv: {
      configureFlags = [ "-fdebug" ];
    });
    reflex-dom-core = pkgs.haskell.lib.dontCheck super.reflex-dom-core;
  };
})

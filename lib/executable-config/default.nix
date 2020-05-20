# This module exposes a tool for several platforms that can be used to inject
# configuration information into a canonical location. It also provides a haskell
# package that can be used to retrieve the injected configuration on each supported
# platform.
{ lib
, runCommand
, obeliskCleanSource
}:

let
  injectConfig = config: assets: runCommand "inject-config" {} (''
    set -x
    mkdir -p $out
    cp --no-preserve=mode -Lr "${assets}" $out/static
    chmod +w "$out"
  '' + lib.optionalString (!(builtins.isNull config)) ''
    if ! mkdir $out/config; then
      2>&1 echo config directory already exists or could not be created
      exit 1
    fi
    cp -a "${config}"/* "$out/config"
    # Needed for android deployments
    find "$out/config" -type f -printf '%P\0' > "$out/config.files"
  '');
in

{
  haskellOverlay = self: super: {
    obelisk-executable-config-lookup = self.callCabal2nix "obelisk-executable-config-lookup" (obeliskCleanSource ./lookup) {};
  };

  platforms = {
    android = {
      # Inject the given config directory into an android assets folder
      inject = injectConfig;
    };
    ios = {
      # Inject the given config directory into an iOS app
      inject = injectConfig;
    };
    web = {
      inject = self: self.callCabal2nix "obelisk-executable-config-inject" (obeliskCleanSource ./inject) {};
    };
  };
}

# This module exposes a tool for several platforms that can be used to inject
# configuration information into a canonical location. It also provides a haskell
# package that can be used to retrieve the injected configuration on each supported
# platform.
{ nixpkgs
, filterGitSource # TODO define this in obelisk
}:
let injectConfig = config: assets: nixpkgs.runCommand "inject-config" {} (''
      set -x
      mkdir -p $out
      cp --no-preserve=mode -Lr "${assets}" $out/static
      chmod +w "$out"
    '' + nixpkgs.lib.optionalString (!(builtins.isNull config)) ''
      if ! mkdir $out/config; then
        2>&1 echo config directory already exists or could not be created
        exit 1
      fi
      cp -a "${config}"/* "$out/config"
    '');
in with nixpkgs.haskell.lib; {
  haskellPackage = self: self.callPackage (filterGitSource ./lookup) {};
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
      inject = self: self.callCabal2nix "obelisk-executable-config-inject" (filterGitSource ./inject) {};
    };
  };
}

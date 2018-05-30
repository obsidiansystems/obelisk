{ hostPlatform, ghc
, mkDerivation, base, bytestring, filepath, stdenv, text, transformers
, obelisk-executable-config-core
, android-activity ? null, jsaddle-wkwebview ? null, ghcjs-dom ? null
}:
let isAndroid = hostPlatform.libc == "bionic";
    isIOS = hostPlatform.isDarwin && hostPlatform.isAarch64;
    isGhcjs = ghc.isGhcjs or false;
in mkDerivation {
  pname = "obelisk-executable-config";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring filepath text transformers obelisk-executable-config-core
  ] ++ (if isAndroid then [
    android-activity
  ] else if isIOS then [
    jsaddle-wkwebview
  ] else if isGhcjs then [
    ghcjs-dom
  ] else [
  ]);
  license = stdenv.lib.licenses.bsd3;
}


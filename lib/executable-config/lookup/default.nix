{ hostPlatform, mkDerivation, base, bytestring, filepath, stdenv, text
, transformers, android-activity, jsaddle-wkwebview
}:
let isAndroid = hostPlatform.libc == "bionic";
    isIOS = hostPlatform.isDarwin && hostPlatform.isAArch64;
in mkDerivation {
  pname = "obelisk-executable-config";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring filepath text transformers
  ] ++ (if isAndroid then [
    android-activity
  ] else if isIOS then [
    jsaddle-wkwebview
  ] else [
  ]);
  configureFlags = if isAndroid then [
    "-fandroid"
  ] else if isIOS then [
    "-fios"
  ] else [
  ];
  license = stdenv.lib.licenses.bsd3;
}


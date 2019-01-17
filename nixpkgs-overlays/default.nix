self: super:

{
  cleanHaskellSource = builtins.filterSource (name: _: let baseName = builtins.baseNameOf name; in !(
    builtins.match "^\\.ghc\\.environment.*" baseName != null ||
    baseName == "cabal.project.local"
  ));

  obeliskExecutableConfig = self.callPackage ../lib/executable-config { };
}

{ mkDerivation, base, bytestring, data-default, diagrams-lib
, diagrams-svg, filepath, lens, lucid, monad-control
, obelisk-asset-serve, raw-strings-qq, semigroups, snap, snap-core
, stdenv, svg-builder, text, transformers-base
}:
mkDerivation {
  pname = "obelisk-snap";
  version = "0.1";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring data-default diagrams-lib diagrams-svg filepath
    lens lucid monad-control obelisk-asset-serve raw-strings-qq
    semigroups snap snap-core svg-builder text transformers-base
  ];
  description = "Project Synopsis Here";
  license = stdenv.lib.licenses.bsd3;
}

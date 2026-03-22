{ pkgs ? import <nixpkgs> {} }:

# Static assets derivation.
# Add build steps here (e.g. postcss, tailwind, sass).
pkgs.stdenv.mkDerivation {
  name = "static";
  src = ./src;
  installPhase = ''
    mkdir -p $out/{css,html,icons,images,js}

    cp -r css $out
    cp -r html $out
    cp -r icons $out
    cp -r images $out
    cp -r js $out
  '';
}

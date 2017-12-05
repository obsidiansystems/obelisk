{ compiler ? "ghc802" }:
let tryReflex = import ../reflex-platform {};
in { pkgs = tryReflex.nixpkgs.pkgs;
     ghcWithPackages = f: tryReflex.nixpkgs.pkgs.haskell.packages.${compiler}.ghcWithPackages (hkgs: with hkgs; [ ghcid ] ++ f hkgs);
}

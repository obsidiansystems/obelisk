self: super:
let
  pkgs = self.callPackage ({ pkgs }: pkgs) { };
  inherit (pkgs.haskell.lib) dontCheck justStaticExecutables;
  inherit (pkgs.haskellPackages) callHackageDirect callHackage;

in
{
  ghcide = justStaticExecutables (dontCheck (callHackageDirect
    {
      pkg = "ghcide";
      ver = "0.2.0";
      sha256 = "199l4qzrghhz6wbfkgqdl4gll4wvgpr190kinzhv88idnn9pxm96";
    }
    rec {
      ghc-check = callHackageDirect
        {
          pkg = "ghc-check";
          ver = "0.3.0.1";
          sha256 = "1dj909m09m24315x51vxvcl28936ahsw4mavbc53danif3wy09ns";
        }
        { };
      lsp-test = dontCheck (callHackage "lsp-test" "0.6.1.0" { });
      haddock-library = dontCheck (callHackage "haddock-library" "1.8.0" { });
      haskell-lsp = dontCheck (callHackageDirect
        {
          pkg = "haskell-lsp";
          ver = "0.22.0.0";
          sha256 = "1q3w46qcvzraxgmw75s7bl0qvb2fvff242r5vfx95sqska566b4m";
        }
        { inherit haskell-lsp-types; });
      haskell-lsp-types = dontCheck (callHackageDirect
        {
          pkg = "haskell-lsp-types";
          ver = "0.22.0.0";
          sha256 = "1apjclphi2v6ggrdnbc0azxbb1gkfj3x1vkwpc8qd6lsrbyaf0n8";
        }
        { });
      regex-tdfa = dontCheck (callHackage "regex-tdfa" "1.3.1.0" {
        regex-base = dontCheck (callHackage "regex-base" "0.94.0.0" { });
      });
      shake = dontCheck (callHackage "shake" "0.18.4" { });
      hie-bios = dontCheck (callHackageDirect
        {
          pkg = "hie-bios";
          ver = "0.5.0";
          sha256 = "116nmpva5jmlgc2dgy8cm5wv6cinhzmga1l0432p305074w720r2";
        }
        { });
    }));
}

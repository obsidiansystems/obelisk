self: super:
  let
    pkgs = self.callPackage ({ pkgs }: pkgs) {};
    inherit (pkgs.haskell.lib) dontCheck;
  in {
  ghcide = pkgs.haskell.lib.justStaticExecutables (dontCheck ((self.override {
    overrides = self: super: {
      ghc-check = self.callHackageDirect {
        pkg = "ghc-check";
        ver = "0.1.0.3";
        sha256 = "038llbvryk5y27jbdpbshp0zw5lw1j6m7qk7vx1n96ykqdzkh649";
      } {};
      lsp-test = dontCheck (self.callHackage "lsp-test" "0.6.1.0" {});
      haddock-library = dontCheck (self.callHackage "haddock-library" "1.8.0" {});
      haskell-lsp = dontCheck (self.callHackage "haskell-lsp" "0.19.0.0" {});
      haskell-lsp-types = dontCheck (self.callHackage "haskell-lsp-types" "0.19.0.0" {});
      regex-posix = dontCheck (self.callHackage "regex-posix" "0.96.0.0" {});
      test-framework = dontCheck (self.callHackage "test-framework" "0.8.2.0" {});
      regex-base = dontCheck (self.callHackage "regex-base" "0.94.0.0" {});
      regex-tdfa = dontCheck (self.callHackage "regex-tdfa" "1.3.1.0" {});
      shake = dontCheck (self.callHackage "shake" "0.18.4" {});
      hie-bios = dontCheck (self.callHackageDirect {
        pkg = "hie-bios";
        ver = "0.4.0";
        sha256 = "19lpg9ymd9656cy17vna8wr1hvzfal94gpm2d3xpnw1d5qr37z7x";
      } {});
    };
  }).callCabal2nix "ghcide" (pkgs.fetchFromGitHub {
    owner = "digital-asset";
    repo = "ghcide";
    rev = "v0.1.0";
    sha256 = "1kf71iix46hvyxviimrcv7kvsj67hcnnqlpdsmazmlmybf7wbqbb";
  }) {}));
}

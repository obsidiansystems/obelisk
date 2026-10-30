# Named nixpkgs overlays. Apply one by name, or all of them with `builtins.attrValues`.
#
# Example:
#   (import nixpkgs { overlays = [ (import ./overlays.nix).brotli ]; }).brotli.version
#   => "1.2.1"  # stock brotli is 1.2.0
#   => "1.2.2"  # stock brotli is 1.2.2
{
  # # Raise brotli to at least 1.2.1.
  # brotli = self: super:
  #   let minBrotliVersion = "1.2.1";
  #
  #       stockBrotliIsOlder = super.lib.versionOlder super.brotli.version minBrotliVersion;
  #
  #       minBrotliSrc = super.fetchFromGitHub {
  #         owner = "google";
  #         repo = "brotli";
  #         tag = "v${minBrotliVersion}";
  #         hash = "sha256-llY72cYffDv0ugsH0p89xIPEbElV4L8u7U/Fe8hT3gI=";
  #       };
  #
  #       minBrotli = super.brotli.overrideAttrs (_: {
  #         version = minBrotliVersion;
  #         src = minBrotliSrc;
  #       });
  #
  #   in {
  #     brotli = if stockBrotliIsOlder then minBrotli else super.brotli;
  #   };
}

# Asset generation pipeline using Nix, which generates a directory structure to be served via obelisk-asset-serve-*

{ nixpkgs
}:

with nixpkgs.lib;

rec {

# Default encoding generation function for this platform; usually zopfliEncodings, but gzipEncodings on darwin due to zopfli not building on darwin.
#
# defaultEncodings :: String -> Derivation
defaultEncodings =
  # zopfli isn't supported on macOS as of nixpkgs eafd703a63
  if nixpkgs.stdenv.isDarwin then gzipEncodings else zopfliEncodings;

# Encoding generation function which uses zopfli to encode the asset with very high compression efficiency, at the cost of CPU time compressing.
# Generates gzip, compress/zlib, and deflate outputs all using zopfli with 5 iterations.
#
# zopfliEncodings :: String -> Derivation
zopfliEncodings = file:
  nixpkgs.stdenv.mkDerivation {
    name = "encodings";

    input = file;

    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out"

      ln -s "$input" "$out/identity"

      zopfli -c --i5 --gzip "$input" >"$out/gzip"
      zopfli -c --i5 --zlib "$input" >"$out/compress"
      zopfli -c --i5 --deflate "$input" >"$out/deflate"
    '';

    buildInputs = [
      nixpkgs.zopfli
    ];
  };

# Encoding generation function which uses gzip to encode the asset with decent compression efficiency and a small CPU cost. Only generates a gzip output.
#
# gzipEncodings :: String -> Derivation
gzipEncodings = file:
  nixpkgs.stdenv.mkDerivation {
    name = "encodings";

    input = file;

    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out"

      ln -s "$input" "$out/identity"

      gzip -c7 "$input" > "$out/gzip"
    '';

    buildInputs = [
      nixpkgs.gzip
    ];
  };

# Encoding generation function which doesn't do any compression.
#
# noEncodings :: String -> Derivation
noEncodings = file:
  nixpkgs.stdenv.mkDerivation {
    name = "encodings";

    input = file;

    builder = builtins.toFile "builder.sh" ''
      source "$stdenv/setup"

      mkdir -p "$out"

      ln -s "$input" "$out/identity"
    '';

    buildInputs = [
    ];
  };

# Convert an attrset into a list of name/value pairs.
#
# setToList :: AttrSet a -> [{ name :: String, value :: a }]
setToList = mapAttrsToList (name: value: { inherit name value; });

# Recursively map some function over a @AttrSet DirEntry@, unioning the results of the application at each level.
#
# unionMapFilesWithName
#   :: ({name :: String, value :: DirEntry} -> {name :: String, value :: a })
#   -> AttrSet DirEntry
#   -> AttrSet DirEntry
unionMapFilesWithName = f: d:
  let go = {name, value}:
        if value.type == "directory"
        then
          [{
            inherit name;
            value = {
              inherit (value) type;
              contents = unionMapFilesWithName f value.contents;
            };
          }]
        else
          setToList (f { inherit name value; });
  in builtins.listToAttrs (builtins.concatLists (map go (setToList d)));

# Read a directory structure recursively into an @AttrSet DirEntry@.
#
# readDirRecursive :: String -> AttrSet DirEntry
readDirRecursive = dir:
  let d = filterAttrs (n: d: !(hasPrefix "." n)) (builtins.readDir dir);
      go = name:
        let path = dir + "/${name}";
            type = d.${name};
        in if type == "directory" then {
          inherit name;
          value = {
            inherit type;
            contents = readDirRecursive path;
          };
        } else {
          inherit name;
          value = {
            inherit type path;
          };
        };
  in builtins.listToAttrs (map go (builtins.attrNames d));

# Render a string as a double quoted string with any internal double quotes or backslashes escaped using backslash, appropriate for a nix expression.
#
# doubleQuoteString :: String -> String
doubleQuoteString = s: "\"" + builtins.replaceStrings ["\\" "\""] ["\\\\" "\\\""] s + "\"";

# Given an @AttrSet DirEntry@ generate a derivation with symlink entries for each @DirEntry@ given.
#
# dirToPath :: AttrSet DirEntry -> Derivation
dirToPath = contents:
  let pairsToBashArray = xs: builtins.concatStringsSep " " (map (np: "[" + doubleQuoteString np.fst + "]=" + doubleQuoteString (toString np.snd)) xs);
      files = pairsToBashArray (mapAttrsToList (fst: entry: { inherit fst; snd = toPath entry; }) contents);
      filesIsLarge = builtins.stringLength files > 65536;
  in
    nixpkgs.stdenv.mkDerivation {
      name = "toPath";
      inherit files;
      passAsFile = optional filesIsLarge "files";
      preferLocalBuild = true;
      builder = builtins.toFile "builder.sh" ''
        set -e
        source "$stdenv/setup"

        eval "declare -A files=(${if filesIsLarge then ''$(cat "$filesPath")'' else ''$files''})"

        mkdir "$out"

        for filename in "''${!files[@]}" ; do
          ln -s "''${files[$filename]}" "$out/$filename"
        done
      '';
    };

# Given a @DirEntry@ either use 'dirToPath' to make a derivation with its contents or return the path directly.
toPath = x: if x.type == "directory" then dirToPath x.contents else x.path;

# Build a DirEntry of the shape { type :: String, contents :: {DirEntry} } with type set to directory. Used to create a data model to pass to toPath.
#
# dir :: String -> DirEntry
dir = contents: {
  type = "directory";
  inherit contents;
};

# Build a DirEntry of the shape { type :: String, path :: String } with type set to symlink. Used to create a data model to pass to toPath.
#
# symlink :: String -> DirEntry
symlink = path: {
  type = "symlink";
  inherit path;
};

# Tests if a given character (string of length 1) is valid for a derivation path name.
# Passing a string of length other than 1 is undefined.
# :: Char -> Bool
isValidDrvNameChar = c: "a" <= c && c <= "z" || "A" <= c && c <= "Z" || "0" <= c && c <= "9" || c == "+" || c == "-" || c == "_" || c == "?" || c == "=" || c == ".";

# Convert a string into a string that could be used in a derivation name.
# replacement lets you choose how to replace invalid characters. It may be any length.
# If it contains invalid characters then the result will also be invalid.
# :: { str :: String, replacement :: String } -> String
mkValidDrvName = { str, replacement ? "?" }:
  let
    newName = stringAsChars (c: if isValidDrvNameChar c then c else replacement) str;
  in if builtins.substring 0 1 newName == "." then "_" + newName else newName;

# Given an encoding generation function and a file entry resulting from readDirRecursive in the form { name :: String, value: { path :: String } },
# build a DirEntry for dirToPath with the various encodings of the asset for dirToPath to build into a final directory tree.
mkAsset = encodings: {name, value}:
  let # We import the file as its own nix store path with a content hash.  If we
      # don't do this, we can wind up with the hash changing depending on other
      # files that are in the same directory as this one.
      fileAlone = builtins.path {
        inherit (value) path;
        name = builtins.unsafeDiscardStringContext (mkValidDrvName { str = builtins.baseNameOf path; });
        recursive = false;
      };
      nameWithHash = builtins.unsafeDiscardStringContext (builtins.baseNameOf fileAlone);
  in {
      ${nameWithHash} = dir {
        type = symlink (builtins.toFile "type" "immutable");
        encodings = symlink (encodings fileAlone);
      };
      ${name} = dir {
        type = symlink (builtins.toFile "type" "redirect");
        target = symlink (builtins.toFile "target" "${nameWithHash}");
      };
    };

# Given an encoding generation function to use and a directory containing assets, recursively walk the directory and encode each asset.
#
# mkAssetsWith :: (String -> Derivation) -> String -> Derivation
mkAssetsWith = encodings: d: unionMapFilesWithName (mkAsset encodings) (readDirRecursive d);

# Given an input directory containing assets, recursively walk the directory and encode each asset with the default encodings.
#
# mkAssets :: String -> Derivation
mkAssets = mkAssetsWith defaultEncodings;

}

{ nixpkgs }:

with nixpkgs.lib;

rec {

zopfliEncodings = file: nixpkgs.stdenv.mkDerivation {
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

gzipEncodings = file: nixpkgs.stdenv.mkDerivation {
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

noEncodings = file: nixpkgs.stdenv.mkDerivation {
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

mapFiles = f: mapFilesWithName ({name, value}: {
  inherit name;
  value = {
    inherit (value) type;
    path = f value.path;
  };
});

mapFilesWithName = f: unionMapFilesWithName (x: { ${x.name} = f x; });

setToList = s: map (name: { inherit name; value = s.${name}; }) (builtins.attrNames s);

unionMapFilesWithName = f: d:
  let go = {name, value}:
        if value.type == "directory" then [{
          inherit name;
          value = {
            inherit (value) type;
            contents = unionMapFilesWithName f value.contents;
          };
        }] else setToList (f {
          inherit name value;
        });
  in builtins.listToAttrs (builtins.concatLists (map go (setToList d)));

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

doubleQuoteString = s: "\"" + builtins.replaceStrings ["\\" "\""] ["\\\\" "\\\""] s + "\"";

dirToPath = contents: toPath {
  type = "directory";
  inherit contents;
};

mapSnds = f: l: zipLists (map (x: x.fst) l) (f (map (x: x.snd) l));

# This crazy function is here to avoid https://github.com/NixOS/nix/issues/875
# It works by creating a dependent chain of derivations that must be built to finish evaluation of the overall nix expression, without evaluating too many.  Based on the current operation of nix, n should not be greater than 1012, and should probably be substantially less.
# chunkDerivations :: Int -> [a] -> [a]
chunkDerivations = n: l:
  let a = take n l;
      b = drop n l;
      go = x: if x == [] then builtins.toFile "emptyList.nix" "[]" else chunkDerivations2 n (take n x) (go (drop n x));
  in a ++ import (go b);

# chunkDerivations2 :: Int -> [a] -> ExprFile [a] -> ExprFile [a]
chunkDerivations2 = n: chunk': next:
  let chunk = map (path: "(import ${builtins.unsafeDiscardOutputDependency path.drvPath}).${path.outputName}") chunk';
  in builtins.seq (builtins.toPath next) (nixpkgs.stdenv.mkDerivation {
  name = "chunkDerivations";
  inherit chunk next;
  preferLocalBuild = true;
  builder = builtins.toFile "chunkDerivations.sh" ''
    source "$stdenv/setup"

    echo $chunk

    echo "[ $chunk ] ++ (import $next)" >"$out"
  '';
});

toPath = x:
  if x.type == "directory" then (
    let files = builtins.concatStringsSep " " (map (np: let n = np.fst; p = np.snd; in "[" + doubleQuoteString n + "]=" + doubleQuoteString (toString p)) (mapSnds (chunkDerivations 128) (map (n: { fst = n; snd = toPath x.contents.${n}; }) (builtins.attrNames x.contents))));
        filesIsLarge = builtins.stringLength files > 65536;
    in nixpkgs.stdenv.mkDerivation {
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
  }) else x.path;

hashFile = path: builtins.readFile (nixpkgs.runCommand "hashFile" {
  buildInputs = [
    nixpkgs.nix
  ];
  preferLocalBuild = true;
  inherit path;
} ''
  nix-hash --flat --base32 --type sha256 "$path" | tr -d '\n' >"$out"
'');

dir = contents: {
  type = "directory";
  inherit contents;
};

symlink = path: {
  type = "symlink";
  inherit path;
};

mkAsset = encodings: {name, value}:
  let nameWithHash = "${hashFile value.path}-${name}";
  in {
    ${nameWithHash} = dir {
      type = symlink (builtins.toFile "type" "immutable");
      encodings = symlink (encodings value.path);
    };
    ${name} = dir {
      type = symlink (builtins.toFile "type" "redirect");
      target = symlink (builtins.toFile "target" "${nameWithHash}");
    };
};

mkAssetsWith = encodings: d: dirToPath (unionMapFilesWithName (mkAsset encodings) (readDirRecursive d));

mkAssets = mkAssetsWith zopfliEncodings;
}

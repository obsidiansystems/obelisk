# obelisk-asset

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/obsidiansystems/obelisk/blob/master/LICENSE)

**Efficiently served, aggressively cached static web resources.**

The `assets.nix` file contains nix expressions that are a mutually recursive set of attributes used to create static asset directories with hashable file encodings. This file should be incorporated within your project's `default.nix` file. (See `example/default.nix`, line 2)

```nix
{ assets ? import ./../assets.nix { inherit nixpkgs; } }:
```

The `mkAssets` and `mkAssetsWith` functions within `assets.nix` only need to be passed an `encoding("zopfliEncodings", "gzipEncodings", "noEncodings")` and a static directory of your choice as arguments.

In this example, `mkAssets` is used on a directory that has a `.png` file inside of it. (See `example/default.nix`, line 11)

```nix
myAssets = assets.mkAssets ./static;
```

Once you have successfully incorporated `assets.nix` into your project's nix file(s), use `nix-build` to generate a symlink of your `mkAssets` expression. The following command from `example/run-example` (line 3) will generate an immutable symlink that holds hashed static assets.

```bash
nix-build -o static.assets -A myAssets
```

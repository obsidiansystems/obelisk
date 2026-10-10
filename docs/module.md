## obelisk\.config\.path

Project config directory (with common/, frontend/, backend/ subtrees)\.
Its common/ and frontend/ subtrees are bundled into the production
server as public configs; backend/ is never bundled (it may hold
secrets; supply those to the running server at runtime)\.



*Type:*
null or absolute path



*Default:*

```nix
null
```



*Example:*

```nix
./config
```



## obelisk\.driver



The nix-haskell driver the project is built with\. The nixpkgs driver
has no wasm compiler of its own\. It reaches the wasm target only
through a ghc-wasm-meta bindist\.



*Type:*
one of “haskell-nix”, “nixpkgs”



*Default:*

```nix
"haskell-nix"
```



*Example:*

```nix
"nixpkgs"
```



## obelisk\.frontend\.js\.package



GHCJS-compiled frontend derivation\.



*Type:*
null or package



*Default:*

```nix
cross-exe { platform = "ghcjs"; package = "frontend"; exe = "frontend"; }
```



*Example:*

```nix
config.haskell-nix.cross-exe { platform = "ghcjs"; package = "admin"; exe = "admin"; }
```



## obelisk\.frontend\.js\.compress



Whether to compress frontend JS with brotli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
false
```



## obelisk\.frontend\.js\.compressed



Compressed frontend jsexe for obelisk-asset-serve-snap\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets optimized
```



*Example:*

```nix
assets.mkAssetsWith assets.noEncodings config.obelisk.frontend.js.optimized
```



## obelisk\.frontend\.js\.optimization\.enable



Whether to run closure-compiler on frontend JS\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
false
```



## obelisk\.frontend\.js\.optimization\.externs



Files passed as --externs\. The jsexe’s own all\.externs\.js goes first\.



*Type:*
list of absolute path



*Default:*

```nix
[ ]
```



*Example:*

```nix
[ ./externs.js ]
```



## obelisk\.frontend\.js\.optimization\.extraFlags



Flags added after the flags closure-compiler declares\.



*Type:*
list of string



*Default:*

```nix
[ ]
```



*Example:*

```nix
[
  "--formatting PRETTY_PRINT"
]
```



## obelisk\.frontend\.js\.optimization\.level



Closure-compiler optimization level\.



*Type:*
one of “BUNDLE”, “WHITESPACE_ONLY”, “SIMPLE”, “TRANSPILE_ONLY”, “ADVANCED”



*Default:*

```nix
"ADVANCED"
```



*Example:*

```nix
"SIMPLE"
```



## obelisk\.frontend\.js\.optimized



Closure-compiled frontend jsexe\.



*Type:*
null or package



*Default:*

```nix
js-optimize { jsexe = "${package}/bin/frontend.jsexe"; }
```



*Example:*

```nix
config.js-optimize { platform = "ghcjs"; package = "admin"; exe = "admin"; jsexe = "${adminJs}/bin/admin.jsexe"; }
```



## obelisk\.frontend\.target



Frontend compilation target\.



*Type:*
one of “js”, “wasm”



*Default:*

```nix
perDriver { haskell-nix = "wasm"; nixpkgs = "js"; }
```



*Example:*

```nix
"js"
```



## obelisk\.frontend\.wasm\.package



WASM-compiled frontend derivation\.



*Type:*
null or package



*Default:*

```nix
cross-exe { platform = "wasi32"; package = "frontend"; exe = "frontend"; }
```



*Example:*

```nix
config.haskell-nix.cross-exe { platform = "wasi32"; package = "admin"; exe = "admin"; }
```



## obelisk\.frontend\.wasm\.compress



Whether to compress frontend WASM with brotli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
false
```



## obelisk\.frontend\.wasm\.compressed



Compressed WASM frontend for obelisk-asset-serve-snap\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets optimized
```



*Example:*

```nix
assets.mkAssetsWith assets.noEncodings config.obelisk.frontend.wasm.optimized
```



## obelisk\.frontend\.wasm\.optimization\.enable



Whether to run wasm-opt on frontend WASM\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
false
```



## obelisk\.frontend\.wasm\.optimization\.extraFlags



Flags passed to wasm-opt after the level\. They replace the declared flags\.



*Type:*
list of string



*Default:*

```nix
[
  "-ol 2"
  "-s 1"
  "--low-memory-unused"
  "--strip-dwarf"
  "--converge"
]
```



*Example:*

```nix
[
  "--converge"
]
```



## obelisk\.frontend\.wasm\.optimization\.level



wasm-opt optimization level (-O)\.



*Type:*
one of “0”, “1”, “2”, “3”, “4”, “s”, “z”



*Default:*

```nix
"2"
```



*Example:*

```nix
"z"
```



## obelisk\.frontend\.wasm\.optimized



Optimized WASM frontend jsexe directory\.



*Type:*
null or package



*Default:*

```nix
wasm-optimize and wasm-jsffi, with the wasi shim
```



*Example:*

```nix
pkgs.runCommand "frontend.jsexe.wasm" {} "cp -r ${./dist-wasm} $out"
```



## obelisk\.static\.compress



Whether to compress static assets with zopfli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



*Example:*

```nix
false
```



## obelisk\.static\.compressed



Static assets preprocessed for obelisk-asset-serve-snap, with optional compression\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets rawStatic
```



*Example:*

```nix
pkgs.runCommand "static" {} "cp -r ${./static-prebuilt} $out"
```



## obelisk\.static\.path



Static assets path or derivation\.



*Type:*
null or absolute path or package



*Default:*

```nix
null
```



*Example:*

```nix
import ./static { inherit pkgs; }
```



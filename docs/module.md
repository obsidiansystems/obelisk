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



## obelisk\.driver



The nix-haskell driver the project is built with\. The nixpkgs driver only supports the js frontend target\.



*Type:*
one of “haskell-nix”, “nixpkgs”



*Default:*

```nix
"haskell-nix"
```



## obelisk\.frontend\.js\.package



GHCJS-compiled frontend derivation\.



*Type:*
null or package



*Default:*

```nix
perDriver obeliskLib.frontendJs config
```



## obelisk\.frontend\.js\.compress



Whether to compress frontend JS with brotli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



## obelisk\.frontend\.js\.compressed



Compressed frontend jsexe for obelisk-asset-serve-snap\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets optimized
```



## obelisk\.frontend\.js\.optimization\.enable



Whether to run closure-compiler on frontend JS\.



*Type:*
boolean



*Default:*

```nix
true
```



## obelisk\.frontend\.js\.optimization\.externs



Extern files passed to closure-compiler via --externs\.



*Type:*
list of absolute path



*Default:*

```nix
[ ]
```



## obelisk\.frontend\.js\.optimization\.extraFlags



Extra flags passed to closure-compiler\.



*Type:*
list of string



*Default:*

```nix
[ ]
```



## obelisk\.frontend\.js\.optimization\.level



Closure-compiler optimization level\.



*Type:*
one of “BUNDLE”, “WHITESPACE_ONLY”, “SIMPLE”, “TRANSPILE_ONLY”, “ADVANCED”



*Default:*

```nix
"ADVANCED"
```



## obelisk\.frontend\.js\.optimized



Closure-compiled frontend jsexe\.



*Type:*
null or package



*Default:*

```nix
closure-compiler frontendJs
```



## obelisk\.frontend\.target



Frontend compilation target\.



*Type:*
one of “js”, “wasm”



*Default:*

```nix
perDriver { haskell-nix = "wasm"; nixpkgs = "js"; }
```



## obelisk\.frontend\.wasm\.package



WASM-compiled frontend derivation\.



*Type:*
null or package



*Default:*

```nix
perDriver obeliskLib.frontendWasm config
```



## obelisk\.frontend\.wasm\.compress



Whether to compress frontend WASM with brotli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



## obelisk\.frontend\.wasm\.compressed



Compressed WASM frontend for obelisk-asset-serve-snap\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets optimized
```



## obelisk\.frontend\.wasm\.optimization\.enable



Whether to run wasm-opt on frontend WASM\.



*Type:*
boolean



*Default:*

```nix
true
```



## obelisk\.frontend\.wasm\.optimization\.extraFlags



Extra flags passed to wasm-opt\.



*Type:*
list of string



*Default:*

```nix
[
  "-ol"
  "2"
  "-s"
  "1"
  "--low-memory-unused"
  "--strip-dwarf"
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



## obelisk\.frontend\.wasm\.optimized



Optimized WASM frontend jsexe directory\.



*Type:*
null or package



*Default:*

```nix
wasm-opt + post-link.mjs
```



## obelisk\.static\.compress



Whether to compress static assets with zopfli/gzip\.



*Type:*
boolean



*Default:*

```nix
true
```



## obelisk\.static\.compressed



Static assets preprocessed for obelisk-asset-serve-snap, with optional compression\.



*Type:*
null or package



*Default:*

```nix
assets.mkAssets rawStatic
```



## obelisk\.static\.path



Static assets path or derivation\.



*Type:*
null or absolute path or package



*Default:*

```nix
null
```



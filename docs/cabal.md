# Building and running an Obelisk app with plain cabal

Obelisk's custom `Setup.hs` hooks cross-compile the frontend, generate the
static assets and link them during an ordinary `cabal build`, so development
needs no nix shell. The edit loop works with cabal alone once the
cross-toolchain is on `PATH`. Production builds should use
`nix-build -A haskell-nix.serverExe.wasm`.

## 1. Prerequisites

The hooks (see `lib/setup/README.md`) shell out, so the following must be on
`PATH`. The hooks find the wasm tools under either target prefix,
`wasm32-unknown-wasi` (haskell.nix) or `wasm32-wasi` (a ghc-wasm-meta
bindist), as the `<prefix>` dispatcher or as `<prefix>-<tool>`:

| Requirement | Used for | Required? |
|---|---|---|
| wasm `cabal` | cross-compiling the frontend to WASM | yes (wasm target) |
| wasm `ghc` | locating `post-link.mjs` (`--print-libdir`) | yes (wasm target) |
| `node` | running `post-link.mjs` to extract the JSFFI bindings | yes (wasm target) |
| `OBELISK_WASI_SHIM` env var | locating the `@bjorn3/browser_wasi_shim` package (its `dist/*.js` is copied into the jsexe) | yes (wasm target); the Setup hook fails if unset |
| `wasm-opt`, `wasm-tools` | optimizing/stripping `frontend.wasm` | optional (skipped with a warning) |
| `javascript-unknown-ghcjs-cabal` | the GHCJS frontend target | only with `-f js` |

The obelisk nix shell provides all of these and exports `OBELISK_WASI_SHIM`.
Entering it once and using plain `cabal` commands inside it is the easiest
setup.

To go fully nix-free:

- Get the WASM toolchain from
  [ghc-wasm-meta](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta),
  which distributes `wasm32-wasi-ghc` and `wasm32-wasi-cabal` binaries.
- Install the WASI browser polyfill and point `OBELISK_WASI_SHIM` at its
  package root:

  ```bash
  npm install @bjorn3/browser_wasi_shim
  export OBELISK_WASI_SHIM="$PWD/node_modules/@bjorn3/browser_wasi_shim"
  ```

- **Caveat:** the skeleton's default `static/generate` implementation runs
  `nix-build`, so an out-of-the-box scaffold touches nix on static-asset
  changes. The framework only requires that the script honor
  `static/generate <output-path>`; any implementation works, including a
  plain `cp -r static/src/. "$1"` or `rsync` for unprocessed assets (see
  section 3).

One more knob: `OBELISK_CROSS_CABAL_ARGS` passes extra arguments through to
the cross cabal invocation. The dev scripts set it to `-O0` plus
`-fno-specialise`-family flags for fast rebuilds; when invoking `cabal`
directly, export it yourself if you want the cross-build to match your native
optimization settings:

```bash
export OBELISK_CROSS_CABAL_ARGS="-O0 --ghc-options=-O0"
```

## 2. The edit loop

```bash
cabal build backend            # native backend + frontend WASM cross-build
cabal run backend              # build + serve on http://localhost:8000
cabal run backend -- --port=8123   # Snap's CLI flags after --
```

- The port comes from Snap's `commandLineConfig` (default 8000); pass any
  Snap option after `--`.
- Run from the project root: the backend reads `config/` relative to its
  working directory. A missing `config/` is fine; configs are optional.
- The frontend target is a flag on `backend`: `wasm` (the default), `js`, or
  `native`. When more than one is set, `native` wins over `js`, and `js` over
  `wasm`.
- `-f native` skips the frontend cross-build entirely (backend-only
  iteration; this is what `ob-repl`/`ob-watch` use).
- `-f js` switches the frontend target to GHCJS
  (`javascript-unknown-ghcjs-cabal` must be on `PATH`).
- The WASM cross-build reuses a persistent `dist-wasm` build directory, so
  only changed modules recompile.

Repl and type-error watching are one-liners over the same mechanism (or use
the `ob-repl` / `ob-watch` scripts, which add the dev ghc-options):

```bash
cabal repl lib:backend lib:common lib:frontend -O0 -f native
ghcid -c "cabal repl lib:backend lib:common lib:frontend -O0 -f native"
```

Inside the repl, `Backend.run` starts the server (the backend executable's
`Main` is a thin wrapper around it).

## 3. Static assets

`static/generated` (and `static/generated/custom` on native builds) carry a
custom `Setup.hs` (`Obelisk.Setup.Static`) which, on every native build:

1. runs the executable script `static/generate <output>` to produce
   `static/generated/data/static`. The script's implementation is yours: the
   only contract is that it writes the assets to the given output path. The
   skeleton's default is `nix-build static/ -o <output>` (building
   `static/default.nix`);
2. hashes the resulting files and regenerates
   `static/generated/src/Obelisk/Generated/Static.hs`, giving you the checked
   `static @"..."` API and immutable `/static/<hash>-name` URLs. This
   manifest step always runs and needs no nix.

**Adding an asset:** drop the file under `static/src/`, rebuild (any
`cabal build backend` re-runs the pipeline), and reference it with
`static @"path/to/file"`. A typo in the path is a compile error.

**Gotcha:** `static/generated/src/Obelisk/Generated/Static.hs` is a tracked
placeholder that builds overwrite, so it can show as modified in
`git status`. Keep `static/generated/data/static` and every other in-tree
build output gitignored: nix builds copy the project source filtered through
its `.gitignore`, and a committed store symlink there breaks the static
manifest generation.

**Going nix-free:** if your `static/` needs no build step, replace the
`nix-build` line in `static/generate` with a plain copy:

```bash
#!/usr/bin/env bash
set -euo pipefail
output="${1:?Usage: static/generate <output-path>}"
rm -rf "$output"
cp -r "$(dirname "$0")/src" "$output"
```

The nix-build version stays useful when static assets are themselves built
(SASS, bundlers, image pipelines: anything expressible as a derivation).

## 4. Production builds without nix (and what you give up)

Plain `cabal build backend` produces everything needed to serve the app:

- the `backend` binary (under `dist-newstyle/`);
- `frontend/data/frontend.jsexe/`: the assembled frontend (`frontend.wasm`,
  `all.js` bootstrap, `ghc_wasm_jsffi.js`, `wasi-shim.js`), symlinked into
  `backend/data/frontend.jsexe` by the backend's Setup hook;
- `backend/data/static`: the generated static assets.

The backend serves these unprocessed directories directly (its
`StaticAssets` configuration carries both a processed and an unprocessed
path, and falls back to the unprocessed one). `wasm-opt` has been applied
if it was on `PATH`. Compared to `nix-build -A haskell-nix.serverExe.wasm`, you do not
get:

- brotli/gzip precompression of assets (the nix `mkAssets` pipeline);
- the content-addressed processed asset layout with far-future caching;
- GHCJS closure-compiler optimization (js target);
- a self-contained output directory with pinned runtime dependencies.

### Manual deploy recipe

This is enough for a small deployment:

```bash
# on the build machine (same OS/arch as the server, or build on the server)
BIN=$(cabal list-bin backend)
rsync -aL "$BIN" backend/data/ config/ me@server:/srv/myapp/
#         ^ binary  ^ frontend.jsexe + static (follow symlinks)  ^ runtime config

# on the server: point the binary's Cabal data dir at the copied assets
cd /srv/myapp
backend_datadir=/srv/myapp ./backend --port=8000
```

(`backend_datadir` is Cabal's standard `Paths_<pkg>` override; the skeleton's
backend resolves its asset paths through `Paths_backend`.) A minimal systemd
unit is just `ExecStart=/srv/myapp/backend --port=8000` with
`WorkingDirectory=/srv/myapp` and `Environment=backend_datadir=/srv/myapp`.

For anything beyond that, use the supported paths: `services.obelisk` (NixOS),
the OCI `containerImage`, `ob-deploy`, or obelisk-systemd; see the README's
Deployment section.

## 5. Docs and Hoogle

- `cabal haddock all` builds package docs; the nix `docs` output builds the
  full set.
- `ob-hoogle` runs `hoogle server` against the Hoogle database on `PATH`.
  Only the nix shell (`withHoogle`) builds one for the dependencies.

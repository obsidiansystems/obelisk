# Migrating from Obelisk v1 to v2

This guide covers moving a project from **Obelisk v1** (the `master` line: GHC
8.10, GHCJS, reflex-platform, the Haskell `ob` CLI) to **Obelisk v2** (the
`next` line: GHC 9.14, WASM **and** GHCJS frontends, `nix-haskell` /
haskell.nix, and a small set of shell scripts in place of the `ob` binary).

v2 is a substantial rewrite. There is no automated upgrade path: the build
system, the developer CLI, and the deployment model all changed. The mapping
below tells you what each old command/attribute became so you can port a
project by hand.

## What changed at a glance

- **Nix backend.** reflex-platform is gone. The build is now a `nix-haskell`
  module (built on haskell.nix). A project is described by a `project.nix`
  module that consumes obelisk's `nix/module.nix`; build outputs come from
  `nix/lib.nix` (`serverExe`, `containerImage`, `frontendWasm`/`frontendJs`).
  See [`docs/module.md`](module.md) for the full option reference.
- **Flakes + submodules.** The repo is a flake and pulls `nix-haskell` and
  `reflex-dom` in as git submodules under `deps/`. Run nix with
  `?submodules=1` (e.g. `nix develop 'git+file:.?submodules=1'`) or clone
  `--recursive`.
- **GHC 8.10 → 9.14.**
- **GHCJS → WASM by default.** The default frontend target is now `"wasm"`
  (compiled with `wasm32-unknown-wasi-cabal`, run via jsaddle-wasm and a
  browser WASI shim). GHCJS is still available as `obelisk.frontend.target =
  "js"`.
- **No more `ob` binary.** The Haskell CLI that lived in `lib/command` was
  deleted. Its day-to-day commands are now small bash scripts in `scripts/`,
  placed on `PATH` by the nix shell. Everything else is plain `cabal` and
  `nix` invocations.
- **Deployment is bring-your-own.** v1's managed `ob deploy` (which provisioned
  and pushed to a remote host) is gone. v2 ships a NixOS service module
  (`services.obelisk`, from `nix/server.nix`) and an OCI container image
  builder (`containerImage`), plus a thin `scripts/ob-deploy` helper. You own
  the host.

## Command and attribute mapping

> Note: `scripts/ob-init` and `scripts/ob-deploy` referenced below are added by
> companion work alongside the existing `scripts/ob-run`, `scripts/ob-repl`,
> and `scripts/ob-hoogle`.

| v1 (`ob` / reflex-platform) | v2 replacement |
|---|---|
| `ob init` | `scripts/ob-init` — scaffolds a new project from `skeleton/`. (Or just `cp -r deps/obelisk/skeleton my-app`.) |
| `ob init --branch BRANCH` / `--symlink PATH` | No managed init source. Copy/point at the skeleton you want; obelisk's own libraries are injected via `obeliskLib.source-repository-packages` in `project.nix`. |
| `ob run` | `ob-run` (`scripts/ob-run`) — watch-and-rebuild dev server on `:8000`; rebuilds on `.hs`/`.cabal`/`.project` change or Enter. |
| `ob watch` | `ob-run`. The separate "watch for errors only" mode is gone; use `ob-run` (rebuild loop) or `ob-repl` + `:reload`. |
| `ob repl` | `ob-repl` (`scripts/ob-repl`) — GHCi for `backend`+`common`+`frontend`, `-O0`, cross builds skipped (`-f -cross`). `ob-repl lib:common` for a single target. |
| `ob hoogle` | `ob-hoogle` (`scripts/ob-hoogle`) — `ob-hoogle start [PORT]` / `stop` / `restart`. |
| `ob shell` | `nix-shell` (or `nix develop 'git+file:.?submodules=1'`). Run a one-off command with `nix-shell --run '...'` / `nix develop -c '...'`. |
| `ob shell --ghcjs` | Cross toolchains are selected by `shell.crossPlatforms` in `project.nix` (e.g. `ps: with ps; [ ghcjs wasi32 ]`) and by `obelisk.frontend.target`. There is no per-invocation `--ghcjs` flag. |
| `ob profile` | Removed. Build with profiling through cabal/nix directly (e.g. a profiling-enabled `cabal build` in the nix shell, or a `--enable-profiling` cabal config). |
| `ob doc` | Removed. Use `ob-hoogle` for searchable docs, or `cabal haddock` / the nix `docs` output. |
| `ob deploy init` | `scripts/ob-deploy` (sets up a deploy directory), **or** wire the `services.obelisk` NixOS module into your host config directly. No managed deploy repo. |
| `ob deploy push` | `scripts/ob-deploy` push step, **or** `nixos-rebuild switch` against a config that imports `serverModule` and sets `services.obelisk.exe = app.serverExe.wasm`. For containers, push the OCI image from `containerImage.wasm`. |
| `ob deploy update` | Bump your source pins by hand: update the `tag`/`rev` in `source-repository-package` stanzas (`cabal.project`) or update the relevant git submodule, then rebuild. There is no managed thunk to "update". |
| `ob deploy test android` | CapacitorJS — wrap the WASM/JS frontend bundle in an Android WebView shell. See [`docs/mobile.md`](mobile.md). |
| `ob deploy test ios` | CapacitorJS — wrap the same bundle in a WKWebView shell. No Apple `TEAMID` flag in obelisk anymore; signing is handled in Xcode/Capacitor. See [`docs/mobile.md`](mobile.md). |
| `ob thunk pack` | Removed. Reference remote deps with native `source-repository-package` stanzas in `cabal.project` (git `location` + `tag`), or as git submodules under `deps/`. |
| `ob thunk unpack` | Removed. With a `source-repository-package`, the dep is already an ordinary git checkout/submodule — edit it in place. |
| `ob thunk update` | Removed. Edit the `tag`/`rev` in the `source-repository-package` stanza (or `cd` into the submodule and check out a new revision), then commit. |
| `ob internal …` | Removed. The dev scripts encapsulate the few internals that mattered (e.g. GHCi configuration is just `ob-repl`). |

## Build commands

| v1 | v2 |
|---|---|
| `nix-build -A exe` (reflex-platform) | `nix-build skeleton -A serverExe.wasm` (or `.js`) |
| reflex-platform `ghcjs` shell build | `nix-build skeleton -A serverExe.js`, or `cabal build` with `obelisk.frontend.target = "js"` |
| — (new) | `nix-build skeleton -A containerImage.wasm` for an OCI image |
| `ob run` triggering a JS build | `cabal build backend` / `cabal run backend` transparently cross-builds the frontend via the custom `Setup.hs` hooks (`OBELISK_CROSS_CABAL_ARGS` passes ghc-options through). |

The nix shell is no longer mandatory for `cabal build` — it only needs
`wasm32-unknown-wasi-cabal` and/or `javascript-unknown-ghcjs-cabal` on `PATH`.

## Deployment model

v1 `ob deploy` managed a deployment git repo, provisioned a remote builder, and
pushed builds for you. v2 deliberately does **not** manage hosts. Pick one of:

### NixOS host (recommended)

Import obelisk's server module and set `services.obelisk`. The module
(`nix/server.nix`) configures nginx (with WebSocket proxy), ACME/Let's Encrypt,
a systemd service with auto-restart, the service user, and firewall rules.

```nix
{ config, ... }:
let app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  imports = [ app.serverModule ];

  services.obelisk = {
    enable = true;
    exe = app.serverExe.wasm;       # or app.serverExe.js
    routeHost = "myapp.example.com";
    enableHttps = true;
    adminEmail = "admin@example.com";
  };
}
```

Apply it with `nixos-rebuild switch` (directly, via `deploy-rs`, or via the
`scripts/ob-deploy` helper). Key options: `routeHost`, `enableHttps`,
`adminEmail`, `internalPort` (default `8000`), `backendArgs`, `redirectHosts`,
`configHash` (bump to force a restart).

### OCI container

```bash
nix-build skeleton -A containerImage.wasm
podman load < result            # or: docker load < result
podman run -p 8000:8000 my-app:latest
```

The image runs `/app/backend --port=8000` and bundles the compressed static and
frontend assets.

### `scripts/ob-deploy`

A thin helper that wraps the above (build `serverExe`/`containerImage` and hand
it to your host) — it replaces the ergonomics of `ob deploy push`/`update`
without the managed deploy repo. It is intentionally minimal; for anything
custom, call `nixos-rebuild`/`podman` yourself.

## Porting checklist

1. Re-scaffold from `skeleton/` (via `scripts/ob-init` or by copying it) and
   move your `backend/`, `common/`, `frontend/`, and `static/` sources across.
2. Replace reflex-platform `default.nix`/thunks with a `project.nix`
   (`nix-haskell` module). Inject obelisk via
   `inherit (obeliskLib) source-repository-packages;`.
3. Convert every `ob thunk` dependency to a `source-repository-package` stanza
   in `cabal.project` or a git submodule under `deps/`.
4. Pick a frontend target (`obelisk.frontend.target`, default `"wasm"`).
5. Replace `ob run`/`ob repl`/`ob hoogle` muscle memory with
   `ob-run`/`ob-repl`/`ob-hoogle`.
6. Replace `ob deploy` with the `services.obelisk` module or the OCI image (see
   above); replace `ob deploy test android|ios` with the Capacitor path in
   [`docs/mobile.md`](mobile.md).

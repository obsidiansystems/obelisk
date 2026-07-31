# Migrating from Obelisk v1 to v2

This guide covers moving a project from **Obelisk v1** (the `master` line: GHC
8.10, GHCJS, reflex-platform, the Haskell `ob` CLI) to **Obelisk v2** (the
`next` line: GHC 9.14, WASM and GHCJS frontends, `nix-haskell` /
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
- **Git submodules.** Obelisk keeps its own dependencies (`nix-haskell`,
  `reflex-dom`) as git submodules under `deps/`, so it has to be pulled in
  recursively. As a dependency it can be cloned with `--recurse-submodules`,
  imported as a flake with `?submodules=1`, or imported as a nix-thunk with
  `fetchSubmodules = true`.
- **GHC 8.10 to 9.14.**
- **GHCJS to WASM by default.** The default frontend target is now `"wasm"`
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
| `ob init` | `nix run github:obsidiansystems/obelisk#init -- my-app` (nothing to clone or install by hand; the scaffold gets obelisk as a git submodule at `deps/obelisk`, pinned to that revision), or `ob-init` from an obelisk shell. |
| `ob init --branch BRANCH` / `--symlink PATH` | No managed init source. Copy/point at the skeleton you want; obelisk's own libraries are injected via `obeliskLib.source-repository-packages` in `project.nix`. |
| `ob run` | `ob-run` (`scripts/ob-run`): watch-and-rebuild dev server on `:8000`; rebuilds on `.hs`/`.cabal`/`.project` change or Enter, cross-compiling the frontend (incrementally) during each rebuild. **Reload model regression:** v1 reloaded interpreted code in-process via ghcid, with the frontend running natively over jsaddle-warp; v2 currently relinks and restarts per change, so dev iteration is slower than v1 for now. Restoring the interpreted dev loop (ghcid + jsaddle-warp, selectable against the real-WASM mode) is the flagship post-2.0 follow-up. v1's `config/common/route` interpretation and dev-TLS are also gone: pass Snap's `--port` after `--`, and use a local reverse proxy (e.g. caddy) for https in development. |
| `ob watch` | `ob-watch` (`scripts/ob-watch`): ghcid over `cabal repl`; type errors on every save at GHCi speed, no server, no cross builds. |
| `ob repl` | `ob-repl` (`scripts/ob-repl`): GHCi for `backend`+`common`+`frontend`, `-O0`, cross builds skipped (`-f native`). `ob-repl lib:common` for a single target. |
| `ob hoogle` | `ob-hoogle` (`scripts/ob-hoogle`): `ob-hoogle start [PORT]` / `stop` / `restart`. |
| `ob shell` | `nix-shell` (or `nix develop`). Run a one-off command with `nix-shell --run '...'` / `nix develop -c '...'`. |
| `ob shell --ghcjs` | Cross toolchains are selected by `shell.crossPlatforms` in `project.nix` (e.g. `ps: with ps; [ ghcjs wasi32 ]`) and by `obelisk.frontend.target`. There is no per-invocation `--ghcjs` flag. |
| `ob profile` | Removed. Build with profiling through cabal/nix directly (e.g. a profiling-enabled `cabal build` in the nix shell, or a `--enable-profiling` cabal config). |
| `ob doc` | Removed. Use `ob-hoogle` for searchable docs, or `cabal haddock` / the nix `docs` output. |
| `ob deploy init` | `scripts/ob-deploy` (sets up a deploy directory), or wire the `services.obelisk` NixOS module into your host config directly. No managed deploy repo. |
| `ob deploy push` | `scripts/ob-deploy` push step, or `nixos-rebuild switch` against a config that imports `serverModule` and sets `services.obelisk.exe = app.serverExe.wasm`. For containers, push the OCI image from `containerImage.wasm`. |
| `ob deploy update` | Bump your source pins by hand: update the `tag`/`rev` in `source-repository-package` stanzas (`cabal.project`) or update the relevant git submodule, then rebuild. There is no managed thunk to "update". |
| `ob deploy test android` | CapacitorJS: wrap the WASM/JS frontend bundle in an Android WebView shell. See [`docs/mobile.md`](mobile.md). |
| `ob deploy test ios` | CapacitorJS: wrap the same bundle in a WKWebView shell. No Apple `TEAMID` flag in obelisk anymore; signing is handled in Xcode/Capacitor. See [`docs/mobile.md`](mobile.md). |
| `ob thunk pack` | The `nix-thunk` CLI: `nix-thunk pack deps/<name>`. Thunks remain first-class for project deps: keep them under `deps/` and consume them in `project.nix` with `source-repository-packages = { my-dep = obeliskLib.thunkSource ./deps/my-dep; };`. Plain `source-repository-package` stanzas in `cabal.project` (git `location` + `tag`) also work. |
| `ob thunk unpack` | `nix-thunk unpack deps/<name>` checks the dep out in place; the nix side keeps working (`thunkSource` handles both packed and unpacked thunks). |
| `ob thunk update` | `nix-thunk update deps/<name>`, or edit the `tag`/`rev` in a `source-repository-package` stanza, then commit. |
| `ob internal ...` | Removed. The dev scripts encapsulate the few internals that mattered (e.g. GHCi configuration is just `ob-repl`). |

## Build commands

| v1 | v2 |
|---|---|
| `nix-build -A exe` (reflex-platform) | `nix-build skeleton -A serverExe.wasm` (or `.js`) |
| reflex-platform `ghcjs` shell build | `nix-build skeleton -A serverExe.js`, or `cabal build` with `obelisk.frontend.target = "js"` |
| (new) | `nix-build skeleton -A containerImage.wasm` for an OCI image |
| `ob run` triggering a JS build | `cabal build backend` / `cabal run backend` transparently cross-builds the frontend via the custom `Setup.hs` hooks (`OBELISK_CROSS_CABAL_ARGS` passes ghc-options through). |

The nix shell is no longer mandatory for `cabal build`, but the cross-build
needs more than the cross cabal: `wasm32-unknown-wasi-cabal`,
`wasm32-unknown-wasi-ghc`, and `node` on `PATH`, plus `OBELISK_WASI_SHIM` set
(and `javascript-unknown-ghcjs-cabal` for the js target). The nix shell
provides all of these; for the fully nix-free setup (ghc-wasm-meta toolchain,
static assets, production builds, manual deploy) see
[`docs/cabal.md`](cabal.md).

## Deployment model

v1 `ob deploy` managed a deployment git repo, provisioned a remote builder, and
pushed builds for you. v2 deliberately does not manage hosts. Pick one of:

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

### obelisk-systemd (multiple apps / home-manager)

For hosts running more than one obelisk app, or for user-level (non-root)
deployment, [`obsidiansystems/obelisk-systemd`](https://github.com/obsidiansystems/obelisk-systemd)
provides NixOS and home-manager modules that turn a built `serverExe` into a
systemd service. It works with v2 unchanged: `serverExe.{wasm,js}` already has
the directory layout it expects (a top-level `backend` binary plus assets), and
it runs `./backend --port=...` just like the built-in module.

```nix
{ ... }:
let app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  # imports = [ obelisk-systemd.nixosModules.default ];  # however you wire the input
  obelisks."my-app" = {
    obelisk = app.serverExe.wasm;          # or .js; a directory, not a bare binary
    configSource = "/var/lib/my-app/config";
    port = 8000;
    enableNginxReverseProxy = true;
    virtualHostName = "myapp.example.com";
    enableHttps = true;
    acmeCertAdminEmail = "admin@example.com";
  };
}
```

`configSource` is the authoritative runtime config: point it at the
project's *full* `config/` (common + frontend + backend, secrets included) kept
on the host outside the Nix store. This complements `obelisk.config.path`: the
build bakes only public `common`/`frontend` into `serverExe`, and `configSource`
supplies everything (including `backend/` secrets) at runtime, so secrets never
enter the store. Other options: `baseUrl` (default `/`, matching v2's relative
routing), `userName`, `userHome`, `extraBackendArgs`.

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
it to your host). It replaces the ergonomics of `ob deploy push`/`update`
without the managed deploy repo. It is intentionally minimal; for anything
custom, call `nixos-rebuild`/`podman` yourself.

## Configuration and `common/route`

The config *mechanism* (`Obelisk.Configs`, `getConfig`/`getTextConfig`, the
`inject`/`lookup` round-trip) is unchanged, but two things around it moved:

**Config is now optional.** In v1 a project needed a `config/` directory, and
`config/common/route` (the app's canonical root URL) was required: `ob run`
refused to start without it and `ob deploy --route` wrote it. In v2 nothing reads
a `common/route` path: routing is relative (`<base href="/">`, host-independent
encoder), so there is no canonical-URL requirement. A missing `config/` yields an
empty config map, no error. The skeleton still *ships* `config/common/route`
(`http://localhost:8000`) as a starter; see `skeleton/config/readme.md`.

**Recovering the `common/route` behavior.** If you relied on it for absolute
URLs (canonical/OpenGraph tags, share links, OAuth redirects), keep the file and
read it explicitly; anywhere you have `HasConfigs` (every frontend widget, and
the backend):

```haskell
import Obelisk.Configs (getTextConfig)
import Obelisk.Route (renderFrontendRoute)
import Common.Route (checkedFullRouteEncoder)

absoluteUrl r = do
  base <- maybe "" T.strip <$> getTextConfig "common/route"
  pure (base <> renderFrontendRoute checkedFullRouteEncoder r)
```

**Public vs secret config, and production.** `getPublicConfigs` still exposes
only the `common/` and `frontend/` subtrees to the page; `backend/` stays
server-side. The new `obelisk.static`-style option `obelisk.config.path` (set to
`./config` in the skeleton `project.nix`) bakes only the public `common`/
`frontend` subtrees into `serverExe`, so `backend/` secrets never enter the Nix
store. Supply secrets at runtime (env, or obelisk-systemd's `configSource`, which
is authoritative; see the deployment section).

**Silent API note:** `Obelisk.ExecutableConfig.Lookup.getConfigs` returns `IO` on
the server but **`JSM`** on the js/wasm frontend. Prefer `HasConfigs` /
`getTextConfig` over calling `getConfigs` directly and you won't notice.

## Static assets

The call-site API is unchanged: `static @"images/logo.png" :: Text`
(TypeApplications + the `StaticFile` symbol class) to `"static/" <> <hash>-name`,
served at `/static/...`; the `$(static "...")` TH form still lives in
`Obelisk.Asset.TH`. What changed is generation.

- **`static/` is now a build derivation.** `static/default.nix` is where you add
  asset build steps (postcss, tailwind, sass); `static/generate` just
  `nix-build`s it. For a no-build-step project, set
  `obelisk.static.path = ./static/src` (raw files) instead. In v1, static was
  typically raw files regenerated by `ob`.
- **`Obelisk.Generated.Static` is regenerated by build hooks, not `ob`.** On a
  native `cabal build`, the custom `Setup.hs` (`Obelisk.Setup.Static`) runs
  `static/generate`, then `gatherHashedPaths` + `writeStaticModule`. Under nix,
  it's generated by `obelisk-asset-manifest-generate --module-only`
  (`staticManifestOverride`). There is no live file-watcher like v1's
  `ob run`; `ob-run` re-triggers the hook on rebuild.
- **Native-generates / cross-reuses split.** `obelisk-generated-static-custom` is
  `buildable: False` for wasm/ghcjs; the cross frontend build reuses the module
  produced by the native/backend build (which runs first).
- **Gotcha:** the skeleton *tracks* placeholder
  `static/generated/src/Obelisk/Generated/Static.hs` and
  `static/generated/data/static`; builds overwrite them, so they can show as
  modified in `git status`.

To port: move your v1 `static/` files under `static/src/` and either keep the
skeleton's `static/default.nix` (a build step) or set
`obelisk.static.path = ./static/src`.

## Breaking changes (compile / build errors when porting)

These fail loudly; expect them:

- **Re-scaffold required.** The project layout changed: new `frontend/js` and
  `frontend/wasm` wrapper packages, `static/generated[/custom]`, `app/Main.hs`
  (was `src-bin/main.hs`), `*/data/.keep` dirs, `project.nix`/`flake.nix`, and a
  `deps/obelisk`. There is no in-place upgrade.
- **Frontend entry point rewrite.** `frontend/app/Main.hs` needs
  `foreign export javascript "hs_start" main :: IO ()` (under `wasm32_HOST_ARCH`
  CPP) and the executable needs reactor link flags
  (`-no-hs-main -optl-mexec-model=reactor "-optl-Wl,--export=hs_start"`). The
  checked encoder is now the top-level `checkedFullRouteEncoder` in
  `Common/Route.hs` rather than a `checkEncoder` call at the entry point.
- **`cabal-version: 3.4` + `-Werror` hardening.** Packages use shared `common`
  stanzas, `-Wunused-packages`, and `-Werror=incomplete-*`/`missing-*`. Expect to
  trim dependency lists and fix incomplete patterns. `subPairRoute`/`subPairRoute_`
  are deprecated in favor of `pairRoute`; under `-Werror` the deprecation is an
  error.
- **GHC 9.14 constraints.** You'll need the `allow-newer`/constraint blocks from
  `cabal.project.config` (`if impl(ghc == 9.14.*)`, plus the `arch(wasm32)` block)
  in your own `cabal.project`.
- **Custom `Setup.hs` hard contracts.** A project must contain a root
  `cabal.project`, an executable `static/generate`, `backend/data/` and
  `frontend/data/` dirs, the cross cabals on `PATH`, and the env vars
  `OBELISK_WASI_SHIM` (hard-fails if unset for WASM) and `OBELISK_CROSS_CABAL_ARGS`.
- **`BackendConfig` gained a field** (`_backendConfig_frontendGhcjsAssets`).
  Positional/record construction breaks; code using `defaultBackendConfig { ... }`
  is fine.
- **Custom `GhcjsWidgets` take a `GhcjsAppUrls` record.**
  `_backendConfig_ghcjsWidgets` is now `GhcjsWidgets (GhcjsAppUrls -> _)`
  instead of `GhcjsWidgets (Text -> _)`: the record carries the all.js URL
  plus the `frontend.wasm` URL when the deployed frontend is a WASM build.
  Apps using `defaultGhcjsWidgets` are unaffected (and gain a
  `frontend.wasm` preload hint on WASM); custom widgets should read
  `_ghcjsAppUrls_allJs` where they previously took the URL directly.
- **reflex-dom is a fork** (`ymeister/reflex-dom`, pinned as a git submodule under
  `deps/`). You cannot pin upstream `reflex-dom`/`reflex-dom-core` yet;
  upstreaming the fork is planned.
- **nix builds are supported on Linux only.** Outputs are exposed for all
  standard systems, but only `x86_64-linux` and `aarch64-linux` are
  tested and served by the binary caches; v1's macOS (`aarch64-darwin`) nix
  support, and the iOS toolchain that depended on it, are gone. Restoring
  macOS support is tracked as a post-promotion follow-up.
- **Raw GHCJS FFI must be ported.** Hand-rolled `foreign import javascript`
  (GHCJS syntax) must move to the GHC WASM backend's JSFFI (or route through
  jsaddle). Code using `ghcjs-dom` is unaffected; it now rides on jsaddle-wasm.

## Silent behavior changes (compile-clean, behave differently)

These won't error; watch for them:

- **`getConfigs` is `IO` on server, `JSM` on the frontend** (js/wasm). Direct
  callers on the frontend need the `JSM` context; `HasConfigs`/`getTextConfig`
  users are unaffected.
- **`<base href="/">` is now unconditional** (the v1 `if os == "ios"` branch is
  gone). A no-op for web, but under a Capacitor `capacitor://`/`file://` origin a
  root-anchored base can break relative asset/route resolution; validate on the
  Capacitor path.
- **"GHCJS" names now govern WASM too.** `all.js`, `frontend.jsexe`,
  `ResourceRoute_Ghcjs`, and `_backendConfig_frontendGhcjsAssets` all apply to the
  WASM target; on WASM, `all.js` is a small bootstrap shim that fetches
  `frontend.wasm`.
- **WASM hydrates** the server-rendered DOM exactly like GHCJS (not a fresh
  client render).
- **~45 `default-extensions` are applied globally** via shared cabal stanzas
  (they were per-module in v1). Notably `OverloadedStrings` and
  `ExtendedDefaultRules` are on everywhere, which can shift literal/defaulting
  behavior in modules that didn't opt in.

## Porting checklist

1. Re-scaffold from `skeleton/` (via `scripts/ob-init` or by copying it) and
   move your `backend/`, `common/`, `frontend/`, and `static/` sources across.
2. Replace reflex-platform `default.nix`/thunks with a `project.nix`
   (`nix-haskell` module). Inject obelisk via
   `inherit (obeliskLib) source-repository-packages;`.
3. Keep `ob thunk` dependencies as nix-thunks under `deps/` (managed with the
   `nix-thunk` CLI, consumed via `source-repository-packages = { my-dep =
   obeliskLib.thunkSource ./deps/my-dep; };` in `project.nix`), or convert
   them to `source-repository-package` stanzas in `cabal.project`.
4. Pick a frontend target (`obelisk.frontend.target`, default `"wasm"`).
5. Replace `ob run`/`ob repl`/`ob hoogle` muscle memory with
   `ob-run`/`ob-repl`/`ob-hoogle`.
6. Replace `ob deploy` with the `services.obelisk` module or the OCI image (see
   above); replace `ob deploy test android|ios` with the Capacitor path in
   [`docs/mobile.md`](mobile.md).
7. Move `static/` files under `static/src/` and keep a `static/default.nix` (or
   set `obelisk.static.path = ./static/src`); see [Static assets](#static-assets).
8. Decide whether you still need `config/common/route`; drop the hard dependency
   and read config via `getTextConfig` where needed; see
   [Configuration and `common/route`](#configuration-and-commonroute).
9. Work through [Breaking changes](#breaking-changes-compile--build-errors-when-porting)
   (they fail loudly) and skim
   [Silent behavior changes](#silent-behavior-changes-compile-clean-behave-differently)
   (they don't).

# Mobile apps on Obelisk v2 (CapacitorJS)

> **Status: UNVALIDATED. This recipe is the intended path; the spike (running
> the bundle in a real WebView on iOS/Android) has not yet been executed.
> Validate before relying on it.**

## How this differs from Obelisk v1

In v1, `ob deploy test android|ios` cross-compiled the Haskell frontend for the
device and packaged it natively (the old `ob deploy test` flow, with an Apple
`TEAMID` for iOS).

In v2 there is no mobile-specific Haskell cross-compile. The frontend
already compiles to a self-contained set of web assets (HTML, JavaScript, and
by default WebAssembly) that run in a browser engine. On a phone that engine
is the system WebView:

- **iOS:** `WKWebView`
- **Android:** the Chromium-based Android System WebView

The mobile story: load the same frontend bundle you ship to the web inside a
WebView wrapper. [CapacitorJS](https://capacitorjs.com/) is that wrapper. It produces an Xcode project and an Android Gradle project
whose web root points at your bundle, and exposes native OS APIs (camera,
filesystem, push, etc.) over a JS bridge that your Haskell frontend can call
through the JavaScript FFI. No Haskell toolchain runs on the device.

Web, iOS, and Android therefore run byte-for-byte the same frontend
artifact. There is nothing mobile-specific to build in Haskell.

## The bundle you are wrapping

The artifact to point Capacitor at is the assembled frontend output directory
(the `jsexe`-style directory).

- **WASM (default).** Built by the wasm32 cross-compile and assembled by
  `Obelisk.Setup.Frontend.Wasm` (`lib/setup/src/Obelisk/Setup/Frontend/Wasm.hs`)
  into `frontend/data/frontend.jsexe/` during `cabal build backend`. The
  assembled directory contains `frontend.wasm`, `ghc_wasm_jsffi.js`, `all.js`
  (the loader/shim, canonical source `lib/setup/data/shim.js`), and the `wasi-shim.js`
  bundle from `@bjorn3/browser_wasi_shim`. (`frontendWasm` in `nix/lib.nix` is an
  internal config-taking function, not a `nix-build -A` target; the
  optimized/compressed variants are `obelisk.frontend.wasm.optimized` /
  `.compressed` in `nix/module.nix`, and the user-facing nix artifact that embeds
  these assets is `serverExe.wasm`.)
- **GHCJS.** If you set `obelisk.frontend.target = "js"`, the equivalent output
  is a classic GHCJS `frontend.jsexe`, assembled the same way into
  `frontend/data/`. This avoids the WASI-shim question below entirely and is the
  safer fallback if the WASM path misbehaves in a WebView.

You also need an `index.html` that loads the bundle (the same entry point the
backend serves). Whatever HTML the backend serves at `/` for the web app is the
HTML the WebView should load.

## Recipe

This is the intended sequence. Treat every step as unverified until the spike
below passes.

### 1. Build the frontend bundle

`cabal build backend` transparently cross-builds the frontend and assembles the
bundle via the Setup.hs hook; this is the simplest way to get the artifact:

```bash
cabal build backend   # cross-builds + assembles the frontend
# frontend/data/frontend.jsexe/ now holds the bundle
```

For a production-optimized bundle, build the server exe and take its embedded
frontend assets:

```bash
nix-build skeleton -A serverExe.wasm   # or serverExe.js for GHCJS
# the result's frontend.jsexe + static dirs are the optimized/compressed bundle
```

To target GHCJS instead of WASM, set `obelisk.frontend.target = "js"` in
`project.nix` (or pass `-f js` to `cabal build backend`) and rebuild.

### 2. Scaffold a Capacitor project

```bash
npm init -y
npm install @capacitor/core @capacitor/cli
npx cap init my-app com.example.myapp --web-dir=www
npm install @capacitor/ios @capacitor/android
```

### 3. Point the web root at the bundle

Copy (or symlink during development) the assembled bundle and its `index.html`
into Capacitor's `web-dir` (`www/` above). The directory must contain
`index.html` plus `frontend.wasm`, `ghc_wasm_jsffi.js`, `all.js`,
`wasi-shim.js` (WASM), or `frontend.jsexe/*` (GHCJS).

```bash
mkdir -p www
cp -rL frontend/data/frontend.jsexe/* www/   # or the serverExe's frontend.jsexe
# ensure www/index.html loads the bundle the same way the web app does
```

### 4. Add platforms and run

```bash
npx cap add ios
npx cap add android
npx cap sync

npx cap run ios          # boots an iOS simulator (needs Xcode)
npx cap run android      # boots an Android emulator (needs Android Studio)
```

Re-run `npx cap sync` after each rebuild of the frontend bundle to copy the new
assets into the native projects.

### Tauri v2 as an alternative

[Tauri v2](https://tauri.app/) also targets iOS and Android and likewise loads
a web bundle into the system WebView. The wrapping approach is the same: point
Tauri's frontend dist at the assembled bundle directory. The same WASI-shim
risk below applies, since Tauri uses the same `WKWebView`/Android WebView
engines.

## Key risk to validate first

**Does jsaddle-wasm + the browser WASI shim run inside `WKWebView`
specifically?** This is the load-bearing unknown and the reason this doc is
marked unvalidated.

The WASM frontend depends on:

- `jsaddle-wasm` driving the DOM from WASM,
- the `@bjorn3/browser_wasi_shim` WASI polyfill loaded via the bootstrap shim (`lib/setup/data/shim.js`)
  (`all.js` / `wasi-shim.js`), located at build time through the
  `OBELISK_WASI_SHIM` env var (see `Obelisk.Setup.Frontend.Wasm` and
  `nix/module.nix`).

These are exercised in desktop browsers, but `WKWebView` is not Safari: it
has historically differed in WASM instantiation limits, module caching,
`WebAssembly.instantiateStreaming` MIME handling, and `SharedArrayBuffer` /
cross-origin-isolation availability. Android System WebView is closer to desktop
Chromium but still version-skewed across devices. The shim's use of streaming
instantiation, file-descriptor emulation, and any threading assumptions are the
most likely failure points.

**Spike to run before trusting this path:** assemble the WASM bundle, load it in
a bare `WKWebView` on the iOS simulator (and Android System WebView on an
emulator), and confirm the app boots, renders, and round-trips an event. If
`WKWebView` chokes on the WASI shim, fall back to `obelisk.frontend.target =
"js"` (GHCJS `frontend.jsexe`), which has no WASM/WASI dependency and is the
conservative WebView target.

## Future: native rendering via Reflex-Lynx

WebView wrapping gives you a web app in a native shell, not native widgets. A
research track aims at native rendering: driving a platform UI toolkit from
Reflex instead of the DOM.

The cited proof of concept is miso-lynx (Miso targeting ByteDance's
[Lynx](https://lynxjs.org/) cross-platform rendering engine). It demonstrates a
Haskell FRP-style frontend rendering to native Lynx views rather than a WebView.
It is not directly Reflex-compatible (Miso and Reflex are different FRP
frameworks), so adopting this path would require a Reflex host for Lynx
(tentatively "Reflex-Lynx"). This is exploratory and not part of the supported
v2 mobile path; the Capacitor/WebView recipe above is the intended route for
now.

# Mobile apps on Obelisk v2 (CapacitorJS)

> **Status: UNVALIDATED. This recipe is the intended path; the spike (running
> the bundle in a real WebView on iOS/Android) has not yet been executed.
> Validate before relying on it.**

## How this differs from Obelisk v1

In v1, `ob deploy test android|ios` cross-compiled the Haskell frontend for
the device. In v2 the frontend compiles to web assets (HTML, JavaScript and by
default WebAssembly) that the system WebView runs:

- **iOS:** `WKWebView`
- **Android:** the Chromium-based Android System WebView

[CapacitorJS](https://capacitorjs.com/) wraps that bundle. It produces an
Xcode project and an Android Gradle project whose web root is your bundle,
and exposes native OS APIs (camera, filesystem, push) over a JS bridge that
the Haskell frontend calls through the JavaScript FFI. Web, iOS and Android
run the same frontend artifact.

## The bundle you are wrapping

The artifact to point Capacitor at is the assembled frontend output directory
(the `jsexe`-style directory).

- **WASM (default).** Built by the wasm32 cross-compile and assembled by
  `Obelisk.Setup.Frontend.Wasm` into `frontend/data/frontend.jsexe/` during
  `cabal build backend`. The directory contains `frontend.wasm`,
  `ghc_wasm_jsffi.js`, `all.js` (the loader, source `lib/setup/data/shim.js`)
  and `wasi-shim.js` from `@bjorn3/browser_wasi_shim`. Under nix, the
  optimized and compressed variants are `obelisk.frontend.wasm.optimized` and
  `.compressed`, and `serverExe.wasm` embeds them.
- **GHCJS.** If you set `obelisk.frontend.target = "js"`, the equivalent output
  is a classic GHCJS `frontend.jsexe`, assembled the same way into
  `frontend/data/`. This avoids the WASI-shim question below entirely and is the
  safer fallback if the WASM path misbehaves in a WebView.

You also need an `index.html` that loads the bundle (the same entry point the
backend serves). Whatever HTML the backend serves at `/` for the web app is the
HTML the WebView should load.

## Recipe

### 1. Build the frontend bundle

`cabal build backend` cross-builds the frontend and assembles the bundle:

```bash
cabal build backend
# frontend/data/frontend.jsexe/ now holds the bundle
```

For an optimized bundle, build the server exe and take its frontend assets:

```bash
nix-build -A haskell-nix.serverExe.wasm   # or .js for GHCJS
# the result's frontend.jsexe and static dirs are the optimized, compressed bundle
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

[Tauri v2](https://tauri.app/) also loads a web bundle into the system
WebView on iOS and Android: point its frontend dist at the bundle directory.
The WASI-shim risk below applies to it too.

## Key risk to validate first

**Does jsaddle-wasm + the browser WASI shim run inside `WKWebView`
specifically?** This is the load-bearing unknown and the reason this doc is
marked unvalidated.

The WASM frontend depends on:

- `jsaddle-wasm` driving the DOM from WASM,
- the `@bjorn3/browser_wasi_shim` WASI polyfill (`wasi-shim.js`), loaded by
  `all.js` and located at build time through `OBELISK_WASI_SHIM`.

Both run in desktop browsers. `WKWebView` has differed from Safari in WASM
instantiation limits, module caching, `instantiateStreaming` MIME handling
and `SharedArrayBuffer` availability, and Android System WebView is
version-skewed across devices. Streaming instantiation, file-descriptor
emulation and any threading assumption in the shim are the likely failure
points.

**Spike to run before trusting this path:** assemble the WASM bundle, load it in
a bare `WKWebView` on the iOS simulator (and Android System WebView on an
emulator), and confirm the app boots, renders, and round-trips an event. If
`WKWebView` chokes on the WASI shim, fall back to `obelisk.frontend.target =
"js"` (GHCJS `frontend.jsexe`), which has no WASM/WASI dependency and is the
conservative WebView target.

## Future: native rendering via Reflex-Lynx

WebView wrapping gives a web app in a native shell. A research track aims at
native widgets: driving a platform UI toolkit from Reflex instead of the DOM.
The proof of concept is miso-lynx, Miso rendering to ByteDance's
[Lynx](https://lynxjs.org/) views. Miso and Reflex are different FRP
frameworks, so this path needs a Reflex host for Lynx ("Reflex-Lynx") first.
The Capacitor recipe above is the supported v2 mobile path.

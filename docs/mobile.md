# Mobile apps on Obelisk v2 (CapacitorJS)

> **Status: the exporter and bundle format are validated in Obelisk's own CI**
> (`webBundle.*` in the release matrix; browser boot/routing/asset assertions
> run in the sibling's workflow). **Native iOS/Android acceptance runs in the
> sibling repository's CI**, whose exercised platform matrix is recorded in
> its [`compatibility.json`](https://github.com/obsidiansystems/obelisk-capacitor/blob/master/compatibility.json).
> A platform not listed there is not claimed to work; simulator/emulator
> proof is what is claimed, not physical-device or signed-store distribution.

## How this differs from Obelisk v1

In v1, `ob deploy test android|ios` cross-compiled the Haskell frontend for
the device. In v2 the frontend compiles to web assets that the system WebView
runs (iOS: `WKWebView`; Android: the Chromium-based Android WebView), wrapped
by [CapacitorJS](https://capacitorjs.com/). Web, iOS and Android run the same
frontend artifact; nothing mobile-specific is compiled in Haskell.

## The portable web bundle

```bash
nix-build default.nix -A nixpkgs.webBundle.js -o result-mobile
# haskell-nix driver: nix-build default.nix -A haskell-nix.webBundle.js
```

The output is a self-contained directory, copyable anywhere and servable as
the web root of any origin:

```text
obelisk-web-bundle.json     {"format":"obelisk-web-bundle","version":1,"target":"js","webDir":"www"}
www/
  index.html                client-bootstrap HTML: public config injected,
                            <meta name="obelisk-frontend-mode" content="client">,
                            and the deferred /ghcjs/all.js entry
  ghcjs/                    complete optimized compiler output, filenames preserved
  static/                   content-hashed files AND original-name aliases,
                            as regular files
```

No symlinks, no `.assets` metadata, no precompressed encodings, no Nix store
references. Only public configuration (`config/common`, `config/frontend`) is
baked into the HTML; `config/backend` is never read, so nothing in the bundle
is secret - treat everything in it as public. Consumers reject unknown
manifest versions/targets and missing payloads.

Booting is selected by the reserved head metadata: `client` renders
immediately from the current path/query; the sibling switches it to
`client-hash`, which routes through the URL hash (`/?q=x#item/a.b`) -
Capacitor's local-server fallback is not a general SPA router, and a dotted
route segment can otherwise be mistaken for an asset. Hash-mode link hrefs,
reloads and back/forward all name the same URL. Without the metadata,
ordinary web behavior (server-side rendering + hydration) is unchanged.

WASM bundles (`haskell-nix.webBundle.wasm`) are produced by the same
exporter; the initial mobile release consumes `target: "js"` only - the
GHC WASM backend's asynchronous JSFFI depends on JSPI, which older
WKWebView/Android WebView releases lack.

## The supported workflow

The Capacitor-specific contract lives in the independently pinned sibling
[`obsidiansystems/obelisk-capacitor`](https://github.com/obsidiansystems/obelisk-capacitor),
so Obelisk's own releases never wait on mobile SDKs and vice versa. Apps pin
it under `deps/obelisk-capacitor`, separately from `deps/obelisk`:

```bash
git clone git@github.com:obsidiansystems/obelisk-capacitor.git deps/obelisk-capacitor
(cd deps/obelisk-capacitor && npm ci)

# 1. Build the bundle (in your Obelisk application)
nix-build default.nix -A nixpkgs.webBundle.js -o result-mobile

# 2. Scaffold the application (needs Node/npm only, not Xcode or the SDK)
node deps/obelisk-capacitor/bin/ob-capacitor.mjs init DIR \
  --app-id com.example.app --app-name "My App" --bundle result-mobile

# 3. Add native projects once (application-owned and source-controlled)
cd DIR && npx cap add android && npx cap add ios   # ios add needs macOS

# 4. After each frontend rebuild: rebuild the bundle, then
node deps/obelisk-capacitor/bin/ob-capacitor.mjs sync \
  --project DIR --bundle result-mobile --platform android
npx cap run android     # or cap open / native build tools
```

The sibling's README documents plugin interop (the `ObeliskCapacitor` jsaddle
boundary, application-owned `native.mjs`), backend origins and CORS
(Capacitor's `capacitor://localhost` / `https://localhost`), signing and
release workflows, and migration from `obelisk-ios-xcode` (whose native C
FFI/WKWebView handles do not carry across).

## Why not just copy the jsexe?

The raw `frontend.jsexe` is not an application: its `index.html` is the GHCJS
stock template (no `<base>`, no injected public config, no static tree), and
the server's processed `*.assets` trees are metadata directories full of
store-absolute links - neither is a portable payload. That is exactly what
`webBundle` assembles, and what the sibling validates before staging.

## Alternatives

[Tauri v2](https://tauri.app/) also loads a web bundle into the system
WebView; point its frontend dist at the bundle directory. The JSPI caveat
above applies to WASM there too.

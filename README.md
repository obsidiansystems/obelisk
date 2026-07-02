# Obelisk

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://github.com/obsidiansystems/obelisk/blob/master/LICENSE)

<p align="center"><img src="docs/obelisk-logo-640.png" width="50%" alt="Obelisk Logo"></p>

Functional reactive web and mobile applications, with batteries included. Obelisk's goal is to represent a cohesive, highly-curated set of choices that [Obsidian Systems](https://obsidian.systems/) has made for building these types of applications in a way that is extremely fast but does not compromise on production readiness.
Supports both WASM and GHCJS frontend targets, GHC 9.14, and deploys to NixOS servers or OCI containers.

- [Overview](#overview)
  - [Who should consider using it?](#who-should-consider-using-it)
- [Quick Start](#quick-start)
- [Project Structure](#project-structure)
- [Development](#development)
  - [ob-init](#ob-init)
  - [ob-run](#ob-run)
  - [ob-repl](#ob-repl)
  - [ob-hoogle](#ob-hoogle)
  - [ob-deploy](#ob-deploy)
- [Nix Module System](#nix-module-system)
  - [project.nix](#projectnix)
  - [Adding packages](#adding-packages)
  - [Package overrides](#package-overrides)
  - [Options](#options)
- [Building](#building)
- [Deployment](#deployment)
  - [NixOS Server](#nixos-server)
  - [OCI Container](#oci-container)
  - [Native Apps](#native-apps)
- [Skeleton](#skeleton)

## Overview

Obelisk allows you to build high-quality web applications very quickly using [Reflex](https://reflex-frp.org/). In minutes you can go from an empty directory to an interactive application running in the browser, all from a single Haskell codebase! The same frontend assets can be packaged as a mobile app with [CapacitorJS](https://capacitorjs.com/) (see [Native Apps](#native-apps)). Obelisk's development environment also enables extremely rapid development and feedback. You can take advantage of Haskell's type system across the frontend and backend boundary. This means changes to your backend that would break your frontend are immediately detected during development and vice versa. Obelisk uses Haskell's compiler to give you a complete "TODO list" of what needs to be updated.

Obelisk is targeted primarily at Haskell developers who want to build high-quality web applications in Haskell, without the distractions of manually choosing and integrating technology for every piece of the system.

### Who should consider using it?

Obelisk assumes basic knowledge of [Haskell](https://www.haskell.org/) and [Reflex/Reflex-DOM](https://reflex-frp.org/), web technologies like [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) and [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS), and a terminal shell like [Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell)). Knowledge of [Nix](https://nixos.org/) helps but is not required for day-to-day development — once the cross-toolchain is on your `PATH`, the edit loop is plain `cabal` (see [docs/cabal.md](docs/cabal.md)). Production builds and the shipped static-asset pipeline do use nix.

## Quick Start

Start from the skeleton or use it as a reference for your own project:

```bash
cp -r deps/obelisk/skeleton my-app
cd my-app
nix-shell  # or: nix develop 'git+file:.'
ob-run
```

Open http://localhost:8000 in your browser.

## Project Structure

A typical obelisk project:

```
my-app/
  backend/             # Snap backend server
  common/              # Shared types and routes
  frontend/            # Reflex-DOM frontend (library + executable)
    js/                # GHCJS cross-compilation wrapper
    wasm/              # WASM cross-compilation wrapper
  static/              # Static assets (css, images, etc.)
    generate           # Static assets generation script
    generated/         # Generated static module (obelisk-generated-static)
      custom/          # Custom Setup.hs for static manifest generation
  cabal.project        # Cabal project file
  project.nix          # Nix-haskell project configuration
  default.nix          # Nix entry point
  flake.nix            # Flake entry point (optional)
```

## Development

Enter the nix shell to get all build tools (GHC, cabal, cross-compilers, hoogle):

```bash
nix-shell  # or: nix develop 'git+file:.'
```

### ob-init

Scaffold a new Obelisk project from the skeleton template into a fresh directory:

```bash
ob-init my-app                 # create ./my-app from the skeleton
ob-init my-app --name my-app   # also set the project name
```

### ob-run

Watch-and-rebuild development server. Rebuilds the backend on `.hs`, `.cabal`, or `.project` file changes, or when you press Enter:

```bash
ob-run
```

Disables optimizations (`-O0`) for fast rebuilds. Cross-compiles the frontend (WASM by default) in the background during each build via the backend's custom Setup.hs.

### ob-repl

Start a REPL with optimizations disabled:

```bash
ob-repl              # loads backend, common, frontend
ob-repl lib:common   # load specific target
```

### ob-watch

Continuous type-error feedback via [ghcid](https://github.com/ndmitchell/ghcid) — recompiles to bytecode on every save and reports errors at GHCi speed, without starting a server or cross-compiling the frontend:

```bash
ob-watch             # watches backend, common, frontend
ob-watch lib:common  # watch a specific target
```

### ob-hoogle

Local Hoogle documentation server:

```bash
ob-hoogle start      # starts on port 8080
ob-hoogle stop
ob-hoogle restart
```

Automatically stopped when exiting the nix shell.

### ob-deploy

Minimal deploy helper that builds the NixOS configuration and activates it on a remote host over SSH:

```bash
ob-deploy <nixos-config> <user@host>
```

`<nixos-config>` is the NixOS system configuration that imports the obelisk server module (see [NixOS Server](#nixos-server)); `<user@host>` is the SSH target to deploy to.

## Nix Module System

Obelisk uses a [nix-haskell](https://github.com/reflex-frp/nix-haskell) module that declares all build configuration as NixOS-style options.

### project.nix

See [`docs/module.md`](docs/module.md) for obelisk-specific options and [`docs/nix-haskell`](docs/nix-haskell) for the full nix-haskell module documentation.

```nix
{ pkgs, obeliskLib, ... }:
{
  name = "my-app";
  src = ./.;

  # Obelisk libraries provided as source-repository-packages
  inherit (obeliskLib) source-repository-packages;

  # Static assets path (hashed + compressed automatically)
  obelisk.static.path = import ./static { inherit pkgs; };

  # Frontend target: "wasm" (default) or "js"
  # obelisk.frontend.target = "wasm";

  shell = {
    crossPlatforms = ps: with ps; [ ghcjs wasi32 ];
    withHoogle = true;
  };
}
```

### Adding packages

Add dependencies to the `build-depends` field in the appropriate `.cabal` file. Nix picks up the corresponding packages from the haskell.nix package set automatically.

To add extra local packages, add the package directory to the `packages:` stanza in `cabal.project` and create a `.cabal` file for it.

### Package overrides

For packages from git, use native `source-repository-package` stanzas in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/someone/some-package
  tag: abc123
```

Use `source-repository-packages` in `project.nix` for local packages, nix-thunks, or git submodules:

```nix
{
  source-repository-packages = {
    some-local-package = ./deps/some-local-package;
    some-remote-package = ./deps/some-remote-package;  # nix-thunk or git submodule
  };
}
```

Use `hackage-overlays` in `project.nix` to make custom packages visible to the nix cabal solver:

```nix
{
  hackage-overlays = [
    {
      name = "some-package";
      version = "0.1.0";
      src = pkgs.fetchFromGitHub {
        owner = "someone";
        repo = "some-package";
        rev = "abc123";
        sha256 = "...";
      };
    }
  ];
}
```

Use `overrides` for haskell.nix module-level overrides (flags, patches, etc.):

```nix
{
  overrides = [
    ({ config, lib, ... }: {
      packages.some-package.flags.some-flag = true;
    })
  ];
}
```

### Options

Key module options (see [`docs/module.md`](docs/module.md) for full reference):

| Option | Default | Description |
|--------|---------|-------------|
| `obelisk.static.path` | `null` | Static assets path or derivation |
| `obelisk.static.compress` | `true` | Compress with brotli + gzip |
| `obelisk.frontend.target` | `"wasm"` | `"wasm"` or `"js"` |
| `obelisk.frontend.js.optimization.enable` | `true` | Run closure-compiler |
| `obelisk.frontend.js.optimization.level` | `"ADVANCED"` | Closure optimization level |
| `obelisk.frontend.wasm.optimization.enable` | `true` | Run wasm-opt |
| `obelisk.frontend.wasm.optimization.level` | `"2"` | wasm-opt -O level |

## Building

### With nix

```bash
# Full production build (backend + optimized/compressed frontend)
nix-build -A serverExe.wasm
nix-build -A serverExe.js
# or: nix build 'git+file:.#serverExe.wasm'

# OCI container image
nix-build -A containerImage.wasm
# or: nix build 'git+file:.#containerImage.wasm'
```

> **Platform note:** the nix builds (and the nix dev shell, which provides the
> cross-compilers) currently support Linux only (`x86_64-linux` and
> `aarch64-linux`). Restoring macOS support is tracked as a follow-up.

### With cabal

```bash
cabal build backend     # native backend
cabal run backend       # run with WASM frontend cross-build
```

The backend's custom Setup.hs automatically cross-compiles the frontend (WASM or GHCJS) and links static assets during `cabal build`.

The nix shell is not strictly required — but the cross-build needs more than the cross cabal: `wasm32-unknown-wasi-cabal`, `wasm32-unknown-wasi-ghc`, and `node` on `PATH`, plus the `OBELISK_WASI_SHIM` environment variable (the nix shell provides and exports all of these). See [docs/cabal.md](docs/cabal.md) for the complete plain-cabal workflow: prerequisites (including getting the toolchain from ghc-wasm-meta without nix), static assets, dev runs, production builds, and a manual deploy recipe.

## Deployment

### NixOS Server

Import the obelisk server module in your NixOS configuration:

```nix
{ config, ... }:
let
  app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  imports = [ app.serverModule ];

  services.obelisk = {
    enable = true;
    exe = app.serverExe.wasm;
    routeHost = "myapp.example.com";
    enableHttps = true;
    adminEmail = "admin@example.com";
    acmeAcceptTerms = true;  # accept the Let's Encrypt / ACME terms of service
  };
}
```

This configures nginx (with WebSocket proxy), ACME/Let's Encrypt, systemd service with auto-restart, and firewall rules.

For hosts running **multiple** obelisk apps, or for user-level (home-manager) deployment, [obelisk-systemd](https://github.com/obsidiansystems/obelisk-systemd) provides NixOS and home-manager modules that turn a built `serverExe` into a systemd service. It works with obelisk v2 unchanged — the `serverExe` directory (top-level `backend` binary + assets) is exactly what it expects:

```nix
{ ... }:
let app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  obelisks."my-app" = {
    obelisk = app.serverExe.wasm;          # a directory, not a bare binary
    configSource = "/var/lib/my-app/config";
    port = 8000;
    enableNginxReverseProxy = true;
    virtualHostName = "myapp.example.com";
    enableHttps = true;
    acmeCertAdminEmail = "admin@example.com";
  };
}
```

Here `configSource` is the authoritative runtime config directory on the host (the full `config/`, secrets included). It pairs with `obelisk.config.path`, which bakes only public `common`/`frontend` config into the build so backend secrets stay out of the Nix store.

### OCI Container

```bash
nix-build -A containerImage.wasm
# or: nix build 'git+file:.#containerImage.wasm'

# Load and run with podman or docker
podman load < result
podman run -p 8000:8000 my-app:latest
```

### Native Apps

Obelisk v2 does **not** cross-compile a native iOS/Android binary the way the legacy reflex-platform pipeline did — that build path has been removed. Instead, the mobile story for v2.0 is [CapacitorJS](https://capacitorjs.com/): a thin native WebView shell wrapped around the **same** WASM frontend web assets (HTML, JS, WASM) that the browser serves. There is no separate Haskell cross-compile for mobile — you ship the web build and Capacitor packages it for iOS and Android, exposing JavaScript hooks for OS features (camera, filesystem, push notifications, etc.) that your Haskell frontend reaches through GHC's JavaScript FFI.

See [`docs/mobile.md`](docs/mobile.md) for the step-by-step recipe.

For desktop, the same web assets can be wrapped with [Tauri](https://tauri.app/) or [ElectronJS](https://www.electronjs.org/) (Capacitor also supports Electron).

#### Future: native mobile via Lynx

Truly native (non-WebView) mobile rendering is a research direction, not a shipping feature. The most promising avenue is a Reflex renderer targeting [Lynx](https://lynxjs.org/). The cited experimental proof-of-concept is [miso-lynx](https://github.com/dmjio/miso), but it is **not** directly usable by Obelisk: miso follows the Elm architecture rather than Reflex's FRP model, so it serves only as evidence that the approach is feasible. A Reflex-Lynx renderer would be required before this becomes a real option.


## Skeleton

The `skeleton/` directory provides a minimal project template. Copy it and modify to create a new project:

```bash
cp -r obelisk/skeleton my-new-app
cd my-new-app
# Edit project.nix, backend/src/Backend.hs, frontend/src/Frontend.hs, etc.
```

The skeleton includes:
- Backend with Snap server and asset serving
- Frontend with Reflex-DOM and route handling
- Common route types
- WASM and GHCJS cross-compilation wrappers
- Static asset generation pipeline
- Nix and flake configuration


## Frequently Asked Questions (FAQ)

Refer to [FAQ](FAQ.md).


## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.

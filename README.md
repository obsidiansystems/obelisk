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

Obelisk assumes basic knowledge of [Haskell](https://www.haskell.org/) and [Reflex/Reflex-DOM](https://reflex-frp.org/), web technologies like [HTML](https://developer.mozilla.org/en-US/docs/Web/HTML) and [CSS](https://developer.mozilla.org/en-US/docs/Web/CSS), and a terminal shell like [Bash](https://en.wikipedia.org/wiki/Bash_(Unix_shell)). Knowledge of [Nix](https://nixos.org/) helps but is not required for day-to-day development: once the cross-toolchain is on your `PATH`, the edit loop is plain `cabal` (see [docs/cabal.md](docs/cabal.md)). Production builds and the shipped static-asset pipeline do use nix.

## Quick Start

Prerequisites: Nix with the reflex-frp binary caches configured; see
[docs/setup.md](docs/setup.md). Without the caches your first build compiles
the toolchain from source.

Scaffold a project without cloning anything by hand:

```bash
nix run github:obsidiansystems/obelisk#init -- my-app
# Nix < 2.27: nix run 'github:obsidiansystems/obelisk?submodules=1#init' -- my-app
cd my-app
nix-shell -A haskell-nix
ob-run
```

Open http://localhost:8000 in your browser. The generated project carries
obelisk as a git submodule at `deps/obelisk`, pinned to the revision it was
scaffolded from and checked out recursively (obelisk keeps its own
dependencies as submodules). Bump it later with `git -C deps/obelisk fetch &&
git -C deps/obelisk checkout <rev> && git -C deps/obelisk submodule update
--init --recursive`.

Both `nix-shell`/`nix-build` and the flake commands build from that
submodule, so clone the project with `--recurse-submodules` (or run `git
submodule update --init --recursive` afterwards); on Nix < 2.27, also add
`?submodules=1` to the project's flake URL.

> **Set this once, on any machine that works with submodule-pinned
> dependencies:**
>
> ```bash
> git config --global submodule.recurse true
> git config --global diff.submodule log
> ```
>
> `submodule.recurse` makes `clone`, `pull`, and `checkout` update submodules
> for you, so switching branches or pulling can't silently leave `deps/` at
> the wrong revision, the single most common way a submodule-pinned project
> builds something you didn't expect. `diff.submodule log` shows a pin bump as
> the commits it moves across instead of two opaque hashes. See
> [docs/setup.md](docs/setup.md#2-git-submodules).

If you have an obelisk checkout (for example to hack on obelisk itself),
`nix-shell -A haskell-nix` in it and run `ob-init my-app` instead; see `ob-init --help`
for the pin-vs-link details.

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
  deps/
    obelisk/           # obelisk itself, as a git submodule
  cabal.project        # Cabal project file
  project.nix          # Nix-haskell project configuration
  default.nix          # Nix entry point
  flake.nix            # Flake entry point (optional)
```

## Development

Enter the nix shell to get all build tools (GHC, cabal, cross-compilers, hoogle):

```bash
nix-shell -A haskell-nix  # or: nix develop
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
ob-run                   # serve on :8000
ob-run -- --port=8017    # args after -- go to cabal run (then the backend)
```

Disables optimizations (`-O0`) for fast rebuilds. Cross-compiles the frontend (WASM by default) in the background during each build via the backend's custom Setup.hs.

### ob-repl

Start a REPL with optimizations disabled:

```bash
ob-repl              # loads backend, common, frontend
ob-repl lib:common   # load specific target
```

### ob-watch

Continuous type-error feedback via [ghcid](https://github.com/ndmitchell/ghcid): recompiles to bytecode on every save and reports errors at GHCi speed, without starting a server or cross-compiling the frontend:

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

### Inputs

Dependencies live under `inputs`, keyed the way flake inputs are. An entry accepts whatever a flake input can be: a flake input, a store path, a checkout, or a packed nix-thunk.

`nixpkgs` and `haskell-nix` come from the pins under `deps/nix-haskell/pins`. The project's own flake inputs are picked up automatically, so following one is enough to override it, and entries of your own are carried through the same way:

```nix
# flake.nix
{
  inputs = {
    obelisk.url = "path:./deps/obelisk";

    # Drop the `follows` to build against a different nixpkgs.
    nixpkgs.follows = "obelisk/nixpkgs";

    # Available in project.nix as config.inputs.some-flake.
    some-flake.url = "github:someone/some-flake";
  };
}
```

An entry set in `project.nix` wins over both, and works without flakes:

```nix
{
  inputs.haskell-nix = ./deps/haskell.nix;   # checkout, or a packed nix-thunk
}
```

Precedence runs pins < flake inputs < `project.nix`. See [`docs/nix-haskell`](docs/nix-haskell) for the full option.

The reflex-dom that obelisk builds against is separate: it comes from `deps/reflex-dom` in the obelisk checkout, and a `reflex-dom` flake input replaces it.

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

Use `source-repository-packages` in `project.nix` for local packages, git submodules, nix-thunks, and flake inputs. A source is anything [`inputs`](#inputs) accepts, so a packed nix-thunk can be given as-is and is resolved to the source it pins; `subdir` selects packages within a source, so a multi-package repository takes one entry rather than one per package:

```nix
{ config, ... }:
{
  source-repository-packages = {
    some-local-package = ./deps/some-local-package;   # local path, git submodule, or nix-thunk

    some-flake-package = config.inputs.some-flake;    # flake input

    some-repo = {
      src = ./deps/some-repo;
      subdir = [ "package-a" "package-b" ];
    };
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

Set per-package options (flags, patches, build hooks) directly in
`project.nix`; they apply to every driver, or under a driver namespace
(`nixpkgs.packages...`, `haskell-nix.packages...`) to that driver only:

```nix
{
  packages.some-package.flags.some-flag = true;

  nixpkgs.packages.other-package.patches = [];
}
```

Use `haskell-nix.overrides` (raw haskell.nix modules) and
`nixpkgs.options.overrides` (raw overlays over the Haskell package set) for
anything the common options do not cover:

```nix
{
  haskell-nix.overrides = [
    ({ config, lib, ... }: {
      packages.some-package.components.library.preBuild = "...";
    })
  ];

  nixpkgs.options.overrides = [
    (self: super: { some-package = pkgs.haskell.lib.dontCheck super.some-package; })
  ];
}
```

### Options

Key module options (see [`docs/module.md`](docs/module.md) for full reference):

| Option | Default | Description |
|--------|---------|-------------|
| `obelisk.driver` | `"haskell-nix"` | `"haskell-nix"` or `"nixpkgs"` |
| `obelisk.static.path` | `null` | Static assets path or derivation |
| `obelisk.static.compress` | `true` | Compress with brotli + gzip |
| `obelisk.frontend.target` | `"wasm"` (`"js"` under nixpkgs) | `"wasm"` or `"js"` |
| `obelisk.frontend.js.optimization.enable` | `true` | Run closure-compiler |
| `obelisk.frontend.js.optimization.level` | `"ADVANCED"` | Closure optimization level |
| `obelisk.frontend.wasm.optimization.enable` | `true` | Run wasm-opt |
| `obelisk.frontend.wasm.optimization.level` | `"2"` | wasm-opt -O level |

## Building

### With nix

Every output is namespaced by the nix-haskell driver that builds it:
`haskell-nix` (the default: haskell.nix toolchain, `wasm` and `js` targets)
or `nixpkgs` (the Haskell infrastructure of nixpkgs). nixpkgs builds no wasm
GHC of its own, so the skeleton gives that driver one from the ghc-wasm-meta
pin. Both drivers then build both targets.

```bash
# Full production build (backend + optimized/compressed frontend)
nix-build -A haskell-nix.serverExe.wasm
nix-build -A haskell-nix.serverExe.js
# or: nix build .#haskell-nix.serverExe.wasm

# The same targets built with the nixpkgs driver
nix-build -A nixpkgs.serverExe.js
nix-build -A nixpkgs.serverExe.wasm

# OCI container image
nix-build -A haskell-nix.containerImage.wasm
# or: nix build .#haskell-nix.containerImage.wasm
```

> **Platform note:** the nix builds (and the nix dev shell, which provides
> the cross-compilers) are supported on Linux only: `x86_64-linux` and
> `aarch64-linux` are tested and served by the binary caches. Other systems
> are exposed but untested; restoring macOS support is tracked as a
> follow-up.

### With cabal

```bash
cabal build backend     # native backend
cabal run backend       # run with WASM frontend cross-build
```

The backend's custom Setup.hs automatically cross-compiles the frontend (WASM or GHCJS) and links static assets during `cabal build`.

The nix shell is not strictly required, but the cross-build needs more than the cross cabal: `wasm32-unknown-wasi-cabal`, `wasm32-unknown-wasi-ghc`, and `node` on `PATH`, plus the `OBELISK_WASI_SHIM` environment variable (the nix shell provides and exports all of these). See [docs/cabal.md](docs/cabal.md) for the complete plain-cabal workflow: prerequisites (including getting the toolchain from ghc-wasm-meta without nix), static assets, dev runs, production builds, and a manual deploy recipe.

## Deployment

For an end-to-end walkthrough (build, host config, deploy, verify, update),
see [guides/deploy](guides/deploy/README.md). The sections below are the
reference for each path.

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
    exe = app.haskell-nix.serverExe.wasm;
    routeHost = "myapp.example.com";
    enableHttps = true;
    adminEmail = "admin@example.com";
    acmeAcceptTerms = true;  # accept the Let's Encrypt / ACME terms of service
  };
}
```

This configures nginx (with WebSocket proxy), ACME/Let's Encrypt, systemd service with auto-restart, and firewall rules.

For hosts running multiple obelisk apps, or for user-level (home-manager) deployment, [obelisk-systemd](https://github.com/obsidiansystems/obelisk-systemd) provides NixOS and home-manager modules that turn a built `serverExe` into a systemd service. It works with obelisk v2 unchanged: the `serverExe` directory (top-level `backend` binary + assets) is exactly what it expects:

```nix
{ ... }:
let app = import ./path/to/my-app { system = "x86_64-linux"; };
in {
  obelisks."my-app" = {
    obelisk = app.haskell-nix.serverExe.wasm;   # a directory, not a bare binary
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
nix-build -A haskell-nix.containerImage.wasm
# or: nix build .#haskell-nix.containerImage.wasm

# Load and run with podman or docker
podman load < result
podman run -p 8000:8000 my-app:latest
```

### Native Apps

Obelisk v2 does not cross-compile a native iOS/Android binary the way the legacy reflex-platform pipeline did; that build path has been removed. The mobile path for v2.0 is [CapacitorJS](https://capacitorjs.com/): a thin native WebView shell wrapped around the same WASM frontend web assets (HTML, JS, WASM) that the browser serves. There is no separate Haskell cross-compile for mobile: you ship the web build and Capacitor packages it for iOS and Android, exposing JavaScript hooks for OS features (camera, filesystem, push notifications, etc.) that your Haskell frontend reaches through GHC's JavaScript FFI.

See [`docs/mobile.md`](docs/mobile.md) for the step-by-step recipe.

For desktop, the same web assets can be wrapped with [Tauri](https://tauri.app/) or [ElectronJS](https://www.electronjs.org/) (Capacitor also supports Electron).

#### Future: native mobile via Lynx

Truly native (non-WebView) mobile rendering is a research direction, not a shipping feature. The most promising avenue is a Reflex renderer targeting [Lynx](https://lynxjs.org/). The cited experimental proof-of-concept is [miso-lynx](https://github.com/dmjio/miso), but it is not directly usable by Obelisk: miso follows the Elm architecture rather than Reflex's FRP model, so it serves only as evidence that the approach is feasible. A Reflex-Lynx renderer would be required before this becomes a real option.


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
- Nix configuration (`default.nix`/`shell.nix`, plus an optional `flake.nix`)


## Frequently Asked Questions (FAQ)

Refer to [FAQ](FAQ.md).


## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.

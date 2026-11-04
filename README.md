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
  - [ob-watch](#ob-watch)
  - [ob-hoogle](#ob-hoogle)
  - [ob-deploy](#ob-deploy)
- [Nix Module System](#nix-module-system)
  - [project.nix](#projectnix)
  - [Inputs](#inputs)
  - [Adding packages](#adding-packages)
  - [Package overrides](#package-overrides)
  - [Options](#options)
  - [Fine-grained builds (Experimental)](#fine-grained-builds-experimental)
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
nix run "git+https://github.com/obsidiansystems/obelisk?submodules=1"#init -- my-app
cd my-app
nix-shell -A haskell-nix
ob-run
```

Open http://localhost:8000 in your browser. The project carries obelisk as a
git submodule at `deps/obelisk`, pinned to the revision it was scaffolded
from, with obelisk's own submodules checked out. Bump the pin with:

```bash
git -C deps/obelisk fetch
git -C deps/obelisk checkout <rev>
git -C deps/obelisk submodule update --init --recursive
```

Every build reads that submodule, so clone the project with
`--recurse-submodules`, or run `git submodule update --init --recursive`
afterwards. On Nix < 2.27, also add `?submodules=1` to the project's flake
URL.

> Set this once on any machine that works with submodule-pinned
> dependencies:
>
> ```bash
> git config --global submodule.recurse true
> git config --global diff.submodule log
> ```
>
> The first keeps `deps/` at the pinned revision across `clone`, `pull`, and
> `checkout`. The second shows a pin bump as the commits it moves across. See
> [docs/setup.md](docs/setup.md#2-git-submodules).

From an obelisk checkout, run `nix-shell -A haskell-nix` and then
`ob-init my-app` instead. `ob-init --help` explains pinning versus linking.

## Project Structure

A typical obelisk project:

```
my-app/
  backend/             # Snap backend server
  common/              # Shared types and routes
  config/              # Runtime config (common/, frontend/, backend/)
  frontend/            # Reflex-DOM frontend (library + executable)
    js/                # GHCJS cross-compilation wrapper
    wasm/              # WASM cross-compilation wrapper
  static/              # Static assets (css, images, etc.)
    generate           # Static assets generation script
    generated/         # Generated static module (obelisk-generated-static)
      custom/          # Custom Setup.hs for static manifest generation
  deps/
    obelisk/           # obelisk itself, as a git submodule
    nix-haskell        # Symlink to obelisk/deps/nix-haskell
    nixpkgs            # Symlink to nix-haskell/pins/nixpkgs
    reflex-dom         # Symlink to obelisk/deps/reflex-dom
  cabal.project        # Cabal project file
  project.nix          # Nix-haskell project configuration
  inputs.nix           # Dependency sources, from deps/ and flake.lock
  default.nix          # Nix entry point
  shell.nix            # Nix shell entry point
  release.nix          # Release matrix (every driver and frontend target)
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

  # Public config (common/ + frontend/) bundled into the production server
  obelisk.config.path = ./config;

  # Frontend target: "wasm" (default) or "js"
  # obelisk.frontend.target = "wasm";

  shell = {
    crossPlatforms = ps: with ps; [ wasi32 ];   # add ghcjs for `-f js` builds
    withHoogle = true;
  };
}
```

### Inputs

Dependencies live under `inputs`, keyed the way flake inputs are. An entry accepts whatever a flake input can be: a flake input, a store path, a checkout, or a packed nix-thunk.

Without flakes, `inputs.nix` builds the set from two sources, the second winning:

1. The inputs of `flake.lock`, when the file exists.
2. Every directory or symlink under `deps/`. A packed nix-thunk resolves to the source it pins. A symlink to a git checkout is fetched with its submodules.

The skeleton's `deps/nixpkgs`, `deps/nix-haskell` and `deps/reflex-dom` are symlinks into the obelisk submodule. nix-haskell's other pins (`haskell-nix`, `ghc-wasm-meta`, `sandstone`) come with it.

The flake commands pass the flake's own inputs instead. The skeleton's `flake.nix` follows obelisk's:

```nix
{
  inputs = {
    self.submodules = true;

    obelisk.url = ./deps/obelisk;
    nix-haskell.follows = "obelisk/nix-haskell";
    nixpkgs.follows = "obelisk/nixpkgs";
    flake-compat.follows = "obelisk/flake-compat";
  };
}
```

To override one, give it a URL in place of the `follows`. An entry of your own is carried through the same way:

```nix
{
  inputs = {
    self.submodules = true;

    obelisk.url = ./deps/obelisk;
    nix-haskell.follows = "obelisk/nix-haskell";
    flake-compat.follows = "obelisk/flake-compat";

    # A different nixpkgs.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    # A different haskell.nix.
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # A different ghc-wasm-meta, the wasm GHC bindist the nixpkgs driver
    # builds the wasm target with.
    ghc-wasm-meta = {
      url = "github:haskell-wasm/ghc-wasm-meta";
      flake = false;
    };

    # A different reflex-dom.
    reflex-dom = {
      url = "github:reflex-frp/reflex-dom/develop";
      flake = false;
    };

    # Available in project.nix as config.inputs.some-flake.
    some-flake.url = "github:someone/some-flake";
  };
}
```

An input the flake declares can also be replaced for one command:

```bash
nix build --override-input haskell-nix github:input-output-hk/haskell.nix
```

An entry set in `project.nix` wins over both, and works without flakes:

```nix
{
  inputs.haskell-nix = ./deps/haskell.nix;   # checkout, or a packed nix-thunk
}
```

Precedence runs flake inputs < `deps/` < `project.nix`. See [`docs/nix-haskell`](docs/nix-haskell) for the full option.

`inputs.reflex-dom` is upstream [reflex-dom](https://github.com/reflex-frp/reflex-dom), from `deps/reflex-dom` or from a `reflex-dom` flake input.

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

### Fine-grained builds (Experimental)

Off by default. nix-haskell can build the native packages one module at a time: sandstone writes one content-addressed derivation per module through Nix dynamic derivations, and the package's own build restores them and only links. A change to one module rebuilds one module. The wasm and js frontends build as before, since cross targets are never built this way.

```nix
{
  fine-grained.enable = true;

  # Every local package by default. Name the ones to build this way:
  # fine-grained.packages = [ "backend" "common" ];

  # A profiling library or haddock reads every module a second time.
  packages.backend.enableLibraryProfiling = false;
  packages.backend.doHaddock = false;
}
```

The builds need a Nix with dynamic derivations, which a stock daemon lacks. `fine-grained.run` wraps one that drives a store of its own under `.nix/store` at the project root, filled from the machine's store by copy. `NIX_DYNAMIC_DRV_STORE` moves that store elsewhere.

```bash
nix-build -A config.fine-grained.run
./result/bin/fine-grained-nix build -f . haskell-nix.serverExe.wasm
```

Add `.nix/` to the project's `.gitignore`, or every nix build copies the store into its source. See [`deps/nix-haskell/README.md`](deps/nix-haskell/README.md#fine-grained-builds) for what makes a plan recompile.

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

Obelisk v2 builds no native iOS or Android binary. The v1 cross-compile path is gone. [CapacitorJS](https://capacitorjs.com/) wraps the frontend web assets the browser serves (HTML, JS, WASM) in a native WebView shell for iOS and Android. It exposes OS features (camera, filesystem, push notifications) over a JavaScript bridge that the Haskell frontend reaches through the JavaScript FFI. Nothing mobile-specific is compiled in Haskell.

See [`docs/mobile.md`](docs/mobile.md) for the recipe.

For desktop, [Tauri](https://tauri.app/) or [ElectronJS](https://www.electronjs.org/) wraps the same assets. Capacitor also supports Electron.

#### Future: native mobile via Lynx

Native rendering without a WebView is a research direction. The candidate is a Reflex renderer for [Lynx](https://lynxjs.org/). [miso-lynx](https://github.com/dmjio/miso) shows the approach works for Haskell. miso follows the Elm architecture, so Obelisk needs a Reflex-Lynx renderer first.


## Skeleton

`skeleton/` is the template `ob-init` copies. Its layout is the one under [Project Structure](#project-structure). Its `deps/obelisk` is a symlink to the obelisk checkout, which `ob-init` replaces with a git submodule, or with `--link` a symlink to a local obelisk.


## Frequently Asked Questions (FAQ)

Refer to [FAQ](FAQ.md).


## Contributing

Contributions and issue reports are encouraged and appreciated! Refer to the [Contributing](CONTRIBUTING.md) guide for information about getting started.

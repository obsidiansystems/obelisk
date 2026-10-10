# Frequently Asked Questions

1. [How do I declare a new Haskell dependency?](#how-do-i-declare-a-new-haskell-dependency)
1. [How do I add or override Haskell dependencies?](#how-do-i-add-or-override-haskell-dependencies)
1. [How do I add more local packages?](#how-do-i-add-more-local-packages)
1. [How do I switch between WASM and GHCJS frontends?](#how-do-i-switch-between-wasm-and-ghcjs-frontends)
1. [How do I disable frontend optimization?](#how-do-i-disable-frontend-optimization)
1. [How do I disable asset compression?](#how-do-i-disable-asset-compression)
1. [How do I fix closure-compiler variable name collisions?](#how-do-i-fix-closure-compiler-variable-name-collisions)
1. [ob-run rebuilds too much. How do I speed it up?](#ob-run-rebuilds-too-much-how-do-i-speed-it-up)
1. [cabal can't find obelisk-setup during cross-compilation](#cabal-cant-find-obelisk-setup-during-cross-compilation)
1. [How do I cache individual build components with Nix?](#how-do-i-cache-individual-build-components-with-nix)
1. [How do I fix "Ambiguous module name" errors?](#how-do-i-fix-ambiguous-module-name-errors)
1. [How do I fix systemd-timesyncd causing my deployment to fail?](#how-do-i-fix-systemd-timesyncd-causing-my-deployment-to-fail)
1. [How do I develop over HTTPS?](#how-do-i-develop-over-https)

### How do I declare a new Haskell dependency?

Add it to `build-depends` in the package's `.cabal` file. Nix picks the package up from the driver's package set, haskell.nix or nixpkgs.

### How do I add or override Haskell dependencies?

For packages from git, use native `source-repository-package` stanzas in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/someone/some-package
  tag: abc123
```

Use `source-repository-packages` in `project.nix` for local packages, git submodules, nix-thunks, and flake inputs. A packed nix-thunk is resolved to the source it pins, so it can be given as-is, and `subdir` selects packages within a source:

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
`project.nix`; under a driver namespace they apply to that driver only:

```nix
{
  packages.some-package.flags.some-flag = true;

  nixpkgs.packages.other-package.patches = [];
}
```

Use `haskell-nix.overrides` (raw haskell.nix modules) and
`nixpkgs.options.overrides` (raw overlays over the Haskell package set) for
anything the common options do not cover.

### How do I add more local packages?

Add the package directory to the `packages:` or `optional-packages:` stanza in `cabal.project` and give it a `.cabal` file. The nix shell, `ob-run` and `ob-repl` pick it up.

### How do I switch between WASM and GHCJS frontends?

In `project.nix`:

```nix
{
  obelisk.frontend.target = "js";   # GHCJS
  # obelisk.frontend.target = "wasm"; # WASM (default)
}
```

Nix builds every target regardless of the default: `haskell-nix.serverExe.wasm`, `haskell-nix.serverExe.js`, `nixpkgs.serverExe.wasm` and `nixpkgs.serverExe.js`.

### How do I disable frontend optimization?

In `project.nix`:

```nix
{
  # Disable closure-compiler for GHCJS
  obelisk.frontend.js.optimization.enable = false;

  # Disable wasm-opt for WASM
  obelisk.frontend.wasm.optimization.enable = false;
}
```

### How do I disable asset compression?

In `project.nix`:

```nix
{
  obelisk.static.compress = false;  # disables compression for both static and frontend
}
```

Or control them independently:

```nix
{
  obelisk.frontend.js.compress = false;   # JS frontend only
  obelisk.frontend.wasm.compress = false; # WASM frontend only
}
```

### How do I fix closure-compiler variable name collisions?

If your static JS files define globals that conflict with closure-compiler's output, use the `obelisk.frontend.js.optimization.externs` option to declare them:

```nix
{
  obelisk.frontend.js.optimization.externs = [ ./externs.js ];
}
```

In `externs.js`, declare the variables that should not be renamed:

```js
var require;
var lib;
```

### ob-run rebuilds too much. How do I speed it up?

`ob-run` already disables GHC optimizations. To skip the frontend cross-build while you work on the backend, build with the `native` flag, as `ob-repl` and `ob-watch` do:

```bash
cabal run backend -f native
```

### cabal can't find obelisk-setup during cross-compilation

Packages with `build-type: Custom` and `setup-depends: obelisk-setup` (backend, frontend-js, frontend-wasm, obelisk-generated-static-custom) must be excluded from cross-compilation builds. The skeleton's `cabal.project` handles this with:

```cabal
if !(arch(javascript) || arch(wasm32))
  packages:
    backend
  optional-packages:
    frontend/js
    frontend/wasm
    static/generated/custom
```

If you see `unknown package: ...:setup.obelisk-setup` during a cross-build, ensure your `cabal.project` has these packages behind the arch conditional.

### How do I cache individual build components with Nix?

The skeleton ships `release.nix`: a matrix of driver and frontend target with a `serverExe`, `containerImage`, `shell` and `shell-build` leaf for each cell, plus `all`. Build the cells you want to cache and push the results:

```bash
nix-build release.nix -A all
nix-build release.nix -A serverExe.haskell-nix.wasm
nix-build release.nix -A shell.nixpkgs
```

Use `nix repl` to explore the attributes.

### How do I fix "Ambiguous module name" errors?

`ob-repl` and `ob-watch` load the `common`, `backend`, and `frontend` packages into one GHCi session, and GHCi does not sandbox their dependencies from each other. A module name provided by two packages can therefore be ambiguous in the repl even though `cabal build` succeeds.

Disambiguate with [PackageImports](https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/package_qualified_imports.html). For example, if you see

```
error:
    Ambiguous module name 'Crypto.Hash':
      it was found in multiple packages:
      cryptohash-0.11.9 cryptonite-0.25
```

specify the package in the import:

```haskell
{-# LANGUAGE PackageImports #-}
import "cryptonite" Crypto.Hash
```

### How do I fix systemd-timesyncd causing my deployment to fail?

An upstream systemd issue can break activation on a NixOS deploy target. Delete `/var/lib/systemd/timesync` and `/var/lib/private` on the target machine; see [issue #670](https://github.com/obsidiansystems/obelisk/issues/670).

### How do I develop over HTTPS?

`ob-run` serves plain HTTP (v1's built-in self-signed dev TLS was removed). Browsers treat `localhost` as a secure context, so most APIs that require HTTPS already work. If you need real TLS locally (for example to test on another device), put a local reverse proxy in front of the dev server:

```bash
caddy reverse-proxy --from localhost:8443 --to localhost:8000
```

[Caddy](https://caddyserver.com/) generates and trusts a local certificate automatically; any TLS-terminating proxy works the same way.

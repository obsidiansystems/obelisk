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

Every component is a standard [cabal](https://www.haskell.org/cabal/) package. Add dependencies to the `build-depends` field in the appropriate `.cabal` file. Nix will automatically pick up the corresponding packages from the haskell.nix package set.

### How do I add or override Haskell dependencies?

For packages from git, use native `source-repository-package` stanzas in `cabal.project`:

```cabal
source-repository-package
  type: git
  location: https://github.com/someone/some-package
  tag: abc123
```

Use `source-repository-packages` in `project.nix` for local packages, nix-thunks, or git submodules:

```nix
{ obeliskLib, ... }:
{
  source-repository-packages = {
    some-local-package = ./deps/some-local-package;      # local path or git submodule
    some-remote-package = obeliskLib.thunkSource ./deps/some-remote-package;  # nix-thunk
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

### How do I add more local packages?

Add the package directory to the `packages:` stanza in `cabal.project` and create a standard `.cabal` file for it. The package will be automatically available in the nix shell and to `ob-run`/`ob-repl`.

### How do I switch between WASM and GHCJS frontends?

In `project.nix`:

```nix
{
  obelisk.frontend.target = "js";   # GHCJS
  # obelisk.frontend.target = "wasm"; # WASM (default)
}
```

For nix builds, `serverExe.wasm` and `serverExe.js` are always both available regardless of the default target.

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

`ob-run` already disables all GHC optimizations (`-O0`, `-fno-specialise`, etc.) for fast rebuilds. If the WASM/GHCJS cross-build is slow, you can disable it temporarily by setting the `cross` flag to false in `backend.cabal`:

```bash
cabal run backend -f-cross
```

This skips the cross-compilation step entirely, useful when you're only working on backend code.

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

Create a `release.nix` that lists the attributes you want to cache:

```nix
let
  project = import ./. {};
in {
  serverExe = project.serverExe.wasm;
  containerImage = project.containerImage.wasm;
  shell = project.shell;
}
```

Then build with `nix-build skeleton -A serverExe` etc. Use `nix repl` to explore available attributes.

### How do I fix "Ambiguous module name" errors?

`ob-repl`, `ob-run`, and `ob-watch` load the `common`, `backend`, and `frontend` packages into one GHCi session, and GHCi does not sandbox their dependencies from each other. A module name provided by two packages can therefore be ambiguous in the repl even though `cabal build` succeeds.

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

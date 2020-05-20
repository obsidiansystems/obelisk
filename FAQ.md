# Frequently Asked Questions

1. [How do I fix invalid entitlements?  ](#how-do-i-fix-invalid-entitlements)
1. [`ob thunk update` or `ob deploy update` fails](#ob-thunk-update-or-ob-deploy-update-fails)
1. [How do I fix `Ambiguous module name` errors?](#how-do-i-fix-ambiguous-module-name-errors)
1. [What does `ob run` actually do?](#what-does-ob-run-actually-do)

### How do I fix invalid entitlements?

You probably did not set `ios.bundleIdentifier` correctly in `default.nix`.
When this happens you'll see an error something like this:

```
2018-11-25 09:34:22.438 ios-deploy[58106:8521046] [ !! ] Error 0xe8008016: The executable was signed with invalid entitlements. AMDeviceSecureInstallApplication(0, device, url, options, install_callback, 0)
```

Fixing the value of `ios.bundleIdentifier` should fix the error.

### `ob thunk update` or `ob deploy update` fails
Whenever an `ob` command fails, try re-running it with `-v`.

If you're using a private repo, and you get a failure in nix-prefetch-url, you may need to unpack and repack the thunk.  Here's some example output that shows this issue:

```
Starting Obelisk </nix/store/j8wls8a89xr6s1a47lg6g83gnbdrfd0l-obelisk-command-0.1/bin/.ob-wrapped> args=["deploy","update","-v"] logging-level=Debug
Creating process: 'nix-build' './.obelisk/impl' '-A' 'command' '--no-out-link'
✔ Built  on ./.obelisk/impl [command]
Handing off to /nix/store/j8wls8a89xr6s1a47lg6g83gnbdrfd0l-obelisk-command-0.1/bin/ob
Starting Obelisk </nix/store/j8wls8a89xr6s1a47lg6g83gnbdrfd0l-obelisk-command-0.1/bin/.ob-wrapped> args=["--no-handoff","deploy","update","-v"] logging-level=Debug
Creating process: 'git' 'ls-remote' '--exit-code' '--symref' 'https://github.com/obsidiansystems/some-private-repo.git' 'refs/heads/master'
git ls-remote maps: (fromList [],fromList [(GitRef_Branch "master","8546dfc8be9653f20bb26214e0991dbb957cb290")])
Latest commit in branch master from remote repo https://github.com/obsidiansystems/some-private-repo.git is 8546dfc8be9653f20bb26214e0991dbb957cb290
Creating process: 'nix-prefetch-url' '--unpack' '--type' 'sha256' 'https://github.com/obsidiansystems/some-private-repo/archive/8546dfc8be9653f20bb26214e0991dbb957cb290.tar.gz'
error: unable to download 'https://github.com/obsidiansystems/some-private-repo/archive/8546dfc8be9653f20bb26214e0991dbb957cb290.tar.gz': HTTP error 404
Process exited with code 1; 'nix-prefetch-url' '--unpack' '--type' 'sha256' 'https://github.com/obsidiansystems/some-private-repo/archive/8546dfc8be9653f20bb26214e0991dbb957cb290.tar.gz'
nix-prefetch-url: Failed to determine sha256 hash of URL https://github.com/obsidiansystems/some-private-repo/archive/8546dfc8be9653f20bb26214e0991dbb957cb290.tar.gz
✖ Updating thunk ./src to latest
```

And here's how you can fix it:

```
ob thunk unpack $SOME_THUNK
ob thunk pack $SOME_THUNK
```
Substitute the directory of your thunk for `$SOME_THUNK`.  In the case of `ob deploy update`, `$SOME_THUNK` will be `src` in the deployment directory.

(Based on issue #351).

### How do I fix `Ambiguous module name` errors?
Since obelisk places the common/backend/frontend modules packages into the same ghci, and ghci doesn't "sandbox" them, it is possible to have conflicting module errors inside `ob repl/watch/run` that do not appear when doing a cabal build.

You can disambiguate this via [PackageImports](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#package-qualified-imports). For instance, if you see
```
error:
    Ambiguous module name ‘Crypto.Hash’:
      it was found in multiple packages:
      cryptohash-0.11.9 cryptonite-0.25
```
then specify the package you want in the import, e.g:
`import "cryptonite" Crypto.Hash`


### What does `ob run` actually do?


#### Short version:

`ob run` starts a [`ghcid`](https://github.com/ndmitchell/ghcid) process which
tries to build your project within a carefully crafted `nix-shell` with all the
project's dependencies and,

* either displays compilation errors/warnings,
* or starts the Obelisk server, which serves:
     * your backend's route handlers,
     * the static assets,
     * the [JSaddle](https://github.com/ghcjs/jsaddle) frontend code (while opening its websocket).

#### Longer version:

Assuming we are in a project created with `ob init`, `ob run` calls (see
`lib/command/src/Obelisk/Command.hs`):

    nix-shell --pure -A shells.ghc --run 'ob --no-handoff internal run-static-io <real-run-function>'

where

* `shells.ghc` is defined in `./default.nix` by importing `./.obelisk/impl/default.nix` which
   is `default.nix` in the present (Obelisk) repository,
* `run-static-io` is a logging-enabled command wrapper
   (cf. `runObelisk` in `lib/command/src/Obelisk/App.hs`).

In this case, it runs the function `Obelisk.Command.Command.run` (defined in
`lib/command/src/Obelisk/Command/Run.hs`).

* It creates a GHCi config from a Nix expression which:
    * loads three packages: `backend`, `common`, `frontend`,
    * obtains a free port number.
* Then runs `ghcid`
    * with a command that reruns `Obelisk.Run.run` at each restart (option
      `--test`).

It is defined in `lib/run/src/Obelisk/Run.hs`:

* It creates a thread which starts the main backend.
  This runs `runSnapWithCommandLineArgs` and passes routes:
    * to the backend (result of the `_backend_run` field of your
      implementation of the `Backend fullRoute frontendRoute` record),
    * or to `serveDefaultObeliskApp` (`lib/backend/src/Obelisk/Backend.hs`) to
      serve static assets.
* Starts `runWidget`
  which itself runs
  [runSettingsSocket](https://hackage.haskell.org/package/warp-3.2.26/docs/Network-Wai-Handler-Warp.html#v:runSettingsSocket)
  (the *“TCP listen loop”*):
    * it binds to TCP socket, and
    * creates the HTTP connection manager
      ( [`newManager`](https://hackage.haskell.org/package/http-client-0.6.3/docs/Network-HTTP-Client.html#v:newManager))
    * calls `obeliskApp` which
        * has a route to serve the [JSaddle](https://github.com/ghcjs/jsaddle) frontend,
        * runs the frontend (*on* the server), to produce pre-rendered pages,
        * starts the JSaddle web-socket,
        * “falls back” to proxying the backend (`fallbackProxy`).

# Frequently Asked Questions

1. [How do I fix invalid entitlements?  ](#how-do-i-fix-invalid-entitlements)
1. [`ob thunk update` or `ob deploy update` fails](#ob-thunk-update-or-ob-deploy-update-fails)
1. [How do I fix `Ambiguous module name` errors?](#how-do-i-fix-ambiguous-module-name-errors)


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

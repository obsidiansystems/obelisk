# Frequently Asked Questions

1. [Why does `ob run` hang in firefox?](#why-does-ob-run-hang-in-firefox)
1. [How do I declare a new Haskell dependency?](#how-do-i-declare-a-new-haskell-dependency)
1. [How do I add or override Haskell dependencies in the package set?](#how-do-i-add-or-override-haskell-dependencies-in-the-package-set)
1. [How do I extend my Obelisk application with more local packages?](#how-do-i-extend-my-obelisk-application-with-more-local-packages)
1. [How do I use `ob run` over HTTPS?](#how-do-i-use-ob-run-over-https)
1. [How do I fix invalid entitlements?](#how-do-i-fix-invalid-entitlements)
1. [`ob thunk update` or `ob deploy update` fails](#ob-thunk-update-or-ob-deploy-update-fails)
1. [How do I fix `Ambiguous module name` errors?](#how-do-i-fix-ambiguous-module-name-errors)
1. [How do I fix `no C compiler provided for this platform` errors?](#how-do-i-fix-no-c-compiler-provided-for-this-platform-errors)
1. [Names of some variables in all.js (produced by GHCJS) collide with already existing static JS files in my project](#names-of-some-variables-in-all.js-(produced-by-ghcjs)-collide-with-already-existing-static-JS-files-in-my-project)
1. [How do I fix systemd-timesyncd causing my deployment to fail?](#how-do-i-fix-systemd-timesyncd-causing-my-deployment-to-fail)
1. [How do I cache individual build components with Nix?](#how-do-i-cache-individual-build-components-with-nix)

### How do I declare a new Haskell dependency?

Every component of your Obelisk application is a standard [cabal](https://www.haskell.org/cabal/) package. That means declaring new Haskell dependencies simply involves updating the `build-depends` field in the appropriate cabal file (`backend.cabal` for example). By default, Obelisk will use its curated package set to choose which version of each package you get. It's possible that the package you need is not already in the curated set or the curated version isn't the one you want. See [How do I add or override Haskell dependencies in the package set?](#how-do-i-add-or-override-haskell-dependencies-in-the-package-set) for a solution to this.

### How do I add or override Haskell dependencies in the package set?

To add a Haskell package that doesn't exist in Obelisk's package set or to override the version of a package, use the `overrides` attribute in your project's `default.nix`. For example, to use a specific version of the `aeson` package fetched from GitHub and a specific version of the `waargonaut` package fetched from Hackage, your `default.nix` will look like:

```nix
# ...
project ./. ({ pkgs, ... }: {
# ...
  overrides = self: super: let
    aesonSrc = pkgs.fetchFromGitHub {
      owner = "obsidiansystems";
      repo = "aeson-gadt-th";
      rev = "ed573c2cccf54d72aa6279026752a3fecf9c1383";
      sha256 = "08q6rnz7w9pn76jkrafig6f50yd0f77z48rk2z5iyyl2jbhcbhx3";
    };
  in
  {
    aeson = self.callCabal2nix "aeson" aesonSrc {};
    waargonaut = self.callHackageDirect {
      pkg = "waargonaut";
      ver = "0.8.0.1";
      sha256 = "1zv28np3k3hg378vqm89v802xr0g8cwk7gy3mr77xrzy5jbgpa39";
    } {};
  };
# ...
```

For further information see [the Haskell section](https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure) of nixpkgs "Contributors Guide".

### How do I extend my Obelisk application with more local packages?

If the standard packages (`frontend`, `backend`, and `common`) are not enough, you may add more local packages by defining them with the `packages` attribute in your `default.nix`. The sources of these packages will be automatically loaded by `ob run`/`ob repl`/etc.

```nix
# ...
project ./. ({ pkgs, ... }: {
# ...
  packages = {
    another = ./dep/another;
  };
# ...
```

### How do I use `ob run` over HTTPS?

To run your app locally over HTTPS, update the protocol in `config/common/route` to `https`, and then use `ob run` as normal.

Obelisk generates a self-signed certificate for running HTTPS so the browser will issue a warning about using an invalid certificate. On Chrome, you can go to `chrome://flags/#allow-insecure-localhost` to enable invalid certificates for localhost.


### How do I fix invalid entitlements?

You probably did not set `ios.bundleIdentifier` correctly in `default.nix`. When this happens you'll see an error something like this:

```
2018-11-25 09:34:22.438 ios-deploy[58106:8521046] [ !! ] Error 0xe8008016: The executable was signed with invalid entitlements. AMDeviceSecureInstallApplication(0, device, url, options, install_callback, 0)
```

Fixing the value of `ios.bundleIdentifier` should fix the error.

### `ob thunk update` or `ob deploy update` fails
Whenever an `ob` command fails, try re-running it with `-v`.

If you're using a private repo, and you get a failure in `nix-prefetch-url`, you may need to unpack and repack the thunk.  Here's some example output that shows this issue:

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

```bash
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

### How do I fix `no C compiler provided for this platform` errors?

This can show up when when doing nix-build (ie for deployment). 

This is probably because you have a package that needs a C compiler (like `snap-core`) in your `common` or `frontend` modules which need to be built for `ghcjs` which does not in fact have a C compiler. You'll need to remove those packages from your cabal files and rework your app to not need them in the frontend.

### Names of some variables in all.js (produced by GHCJS) collide with already existing static JS files in my project
Obelisk now allows the addition of a file to resolve such name collision errors. The file's path can be passed in the configuration via the **externjs** key, as shown below:
```nix
project ./. ({ pkgs, ... }: with pkgs.haskell.lib; {
  staticFiles = ...
  externjs = path/to/file
})
```

This file should have declarations for the global variables that are needed by your static JS files, for example:
```haskell
var require = false;
var lib = false;
```

Any variables defined in this file will not be used in the minification process.

### Why does ob run hang in firefox?

`ob run` runs the project frontend through the jsaddle library, instead of compiling it to javascript. There is an issue with the way that jsaddle interfaces with the browser which causes `ob run` to fail to operate on some browsers. For more information, see [this issue](https://github.com/ghcjs/jsaddle/issues/64).

### How do I fix systemd-timesyncd causing my deployment to fail?

This is an upstream issue that can be resolved by deleting `/var/lib/systemd/timesync` and `/var/lib/private` on the machine targeted for deployment, as per [issue #670](https://github.com/obsidiansystems/obelisk/issues/670).

### How do I cache individual build components with Nix?

> I can do `nix-build -A exe` and the result will come completely from cache. But if I follow that up with `nix-build -A ghc.backend` or `nix-build -A ghc.frontend` and the result is not cached. Shouldn't the former have already built the latter?

The succinct answer is that `ghc.backend` is not statically linked and `ghc.frontend` is not a GHCJS build. Additionally `ghcjs.frontend` is not closure compiled, so the derivations are not the same and will result in a new build being triggered.

Our recommendation is to have a nix file in your project, such as `release.nix`, that lists all of the attributes you'd like to cache.

For example, you could do something like:
```nix
{ }:
let
  myProject = import ./. {};
in {
  android = myProject.android.frontend; # Android application
  exe = myProject.exe;                  # Compiled backend and ghcjs frontend
  shell = myProject.shells.ghc;         # Project shell
}
```
This gives you direct control over what's built and cached.

To see what else can be added to this list, run `nix repl` in your project directory and type: `a = import ./. {}` then look at the value of `a` via TAB completion to see the components of your obelisk project.

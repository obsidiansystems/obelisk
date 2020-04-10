# Revision history for obelisk

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

## Unreleased

* ([#674](https://github.com/obsidiansystems/obelisk/pull/674)) Introduce a new thunk format to support accessing the thunk's source directly. Previously thunks only supported sources that contained `default.nix`. With the latest format both packed and unpacked thunks have a similar directory structure:

  Packed thunks look like

      my-thunk/
      ├── default.nix
      ├── thunk.nix
      └── github.json

  and unpacked thunks look like

      my-thunk/
      ├── default.nix
      ├── thunk.nix
      └── local
          └── ... # checkout of your dependency

  Instead of unpacked thunks *replacing* your thunk files, the source is placed in `local`.

  This format provides the new `thunk.nix` interface regardless of whether the thunk is packed or unpacked. With this you can always access the raw source of your thunk from nix with `import my-thunk/thunk.nix`. `thunk.nix` always uses your local copy if it's available or fetches the source described in `github.json`/`git.json` if it's not.

  To convert a `git` checkout into a thunk you can use the new `ob thunk init` command. `ob thunk pack` will automatically perform `ob thunk init` on raw `git` checkouts.

  Note that this format is backwards compatible since `import ./my-thunk` works the same way both before and after this change.
* ([#665](https://github.com/obsidiansystems/obelisk/pull/665)) Add `--interpret` and `--no-interpret` options to `ob run`/`ob watch`/`ob repl`/`ob shell`. These options allow you to pick which paths will be pre-compiled by `nix` when entering the shell/session and which won't. For example `ob run --no-interpret dep` will ensure that any dependencies found in `./dep` will be built by `nix` before loading the rest of the project into the `ghci` session. The same configuration for `ob shell` will ensure that those packages are built and available in the `ghc` package database inside the shell.

  **NOTE:** `ob shell`'s default behavior is now different. By default it now behaves like `ob run`/`ob watch`/`ob repl` in that it does *not* pre-build any packages whose `.cabal` or `package.yaml` files are found in the project. To regain the previous behavior, use `ob shell --no-interpret . --interpret backend --interpret common --interpret frontend` from the project root.
* ([#695](https://github.com/obsidiansystems/obelisk/pull/695)) `ob deploy init` now requires that your obelisk project be a clean `git` checkout with pushed changes. It has always required that your obelisk project be a `git` repository, but it did not require that your local changes be committed and pushed. This new requirement is added to ensure users don't accidentally create a deployment pointing to an old version of the project.
* ([#693](https://github.com/obsidiansystems/obelisk/pull/693)) Fix a bug where some packages in `.attr-cache` directories would be incorrectly picked up and used instead of the correct ones when using `ob run`/`ob watch`/`ob repl`.

## v0.7.0.1

* Fix the version number for `ob` the command-line tool. ([#679](https://github.com/obsidiansystems/obelisk/pull/679))

## v0.7.0.0

* Fully support HTTP [Range](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Range) requests on static assets to support Safari. ([#664](https://github.com/obsidiansystems/obelisk/pull/664))
* Support non-EC2 deployments. ([#658](https://github.com/obsidiansystems/obelisk/pull/658))
* Fix `ob deploy test android` to work. ([#645](https://github.com/obsidiansystems/obelisk/pull/645))
* Fix vulnerability where Android deployments would leave signing keys in the nix store which is world readable. ([#645](https://github.com/obsidiansystems/obelisk/pull/645)) (Thanks to [kmicklas](https://github.com/kmicklas) for the report.)
* Add `Obelisk.Backend.runBackendWith` to allow several customizations. ([#668](https://github.com/obsidiansystems/obelisk/pull/668), [#644](https://github.com/obsidiansystems/obelisk/pull/644))
* Add `ob profile` command to run Obelisk projects with profiling. `ob profile` works like `ob run`, but instead of using `ghci`, it builds an executable that is built with profiling enabled. ([#654](https://github.com/obsidiansystems/obelisk/pull/654))
* Obelisk's `default.nix` now exposes `mkAssets` function which is used to construct the assets served by an Obelisk application. ([#651](https://github.com/obsidiansystems/obelisk/pull/651))
* Bump reflex-platform to v0.5.2.0. ([#671](https://github.com/obsidiansystems/obelisk/pull/671))

## v0.6.0.0 - 2020-02-21

* Fix a bug in `Obelisk.Route.Frontend` where `routeLink`, `routeLinkDynAttr`, and `dynRouteLink` would not behave exactly like `<a href="...">` when run by JavaScript. These functions now scroll to the top of the page when the link is clicked. ([#540](https://github.com/obsidiansystems/obelisk/pull/540))
* Fix a bug in `ob run`/`ob watch`/`ob repl` where nested Obelisk projects would also be loaded into the session. These are now ignored. ([#652](https://github.com/obsidiansystems/obelisk/pull/652))
* Improve behavior of `ob run`/`ob watch`/`ob repl` when multiple packages with the same name are encountered. Obelisk now issues a warning and tells you which one it will use. ([#653](https://github.com/obsidiansystems/obelisk/pull/653))
* Removed `Obelisk.Backend.mkRouteToUrl` since it is easily written in terms of `Obelisk.Route.renderObeliskRoute`:

      mkRouteToUrl validFullEncoder (k :/ v) = renderObeliskRoute validFullEncoder (FullRoute_Frontend (ObeliskRoute_App k) :/ v)

* Add `Obelisk.Backend.renderAllJsPath` to expose URL path to `ghcjs/all.js`. ([#545](https://github.com/obsidiansystems/obelisk/pull/545))
* Add argument to `serveDefaultObeliskApp`, `serveObeliskApp`, and `serveGhcjsApp` to take the path to `all.js` instead of hard-coding it. ([#545](https://github.com/obsidiansystems/obelisk/pull/545))

## v0.5.0.0 - 2020-02-07

* Add `Obelisk.Route.(?/)`, a convenience function for constructing routes nested in `Maybe`. ([#457](https://github.com/obsidiansystems/obelisk/pull/457))
* Add local unpacked packages to the `ob run`, `ob watch`, and `ob repl` sessions. Any `.cabal` or hpack package inside the current obelisk project will be loaded into the session. For `ob run`/`ob watch` this means the session will automatically reload when you save a source file in any of those packages. For `ob repl` it means that `:r` will reload changes to any of those packages. There are some edge cases where this integration is still rough. Report any issues you encounter. ([#489](https://github.com/obsidiansystems/obelisk/pull/489))
* Add `ob hoogle` command to start a local [Hoogle](https://hoogle.haskell.org/) server for the project. ([#628](https://github.com/obsidiansystems/obelisk/pull/628))
* `ob thunk pack` will now attempt to automatically detect if the thunk is a private or public repo. To avoid this detection, specify `--private` or `--public` manually. ([#607](https://github.com/obsidiansystems/obelisk/pull/607))
* Fix a bug in the plain git thunk loader for thunks marked as 'private' when the revision is not in the default branch. ([#648](https://github.com/obsidiansystems/obelisk/pull/648))
* Improve handling of runtime nix dependencies. This may fix some issues encountered particularly by users on systems other than NixOS.

## v0.4.0.0 - 2020-01-10

* Bump reflex-platform which, notably, bumps nixpkgs to 19.09. ([#585](https://github.com/obsidiansystems/obelisk/pull/585))
* Add new thunk loader for Git repositories that supports `file://` Git remotes and supports private repositories via `builtins.fetchGit` for private repositories (when the `git.json` file specifies `"private": true`). ([#594](https://github.com/obsidiansystems/obelisk/pull/594))
* Add a new thunk loader for GitHub repositories that uses `builtins.fetchTarball` for public repositories to increase loader performance and uses `fetchFromGitHub` for private repositories (when the `github.json` file specifies `"private": true`). Note that `fetchFromGitHub` requires some Nix configuration for the Nix builder to access the repository. If `ob thunk pack` fails in this case, use `-v` to see Nix's helpful message. ([#594](https://github.com/obsidiansystems/obelisk/pull/594))
* Add `--public`/`--private` options to `ob thunk pack` to specify if a repository should be treated as a public or private. ([#594](https://github.com/obsidiansystems/obelisk/pull/594))
* Improve error messaging when a dependency doesn't have the expected `.cabal` or `package.yaml` file. ([#597](https://github.com/obsidiansystems/obelisk/pull/597))
* Improve the skeleton in small ways. ([#593](https://github.com/obsidiansystems/obelisk/pull/593), [#589](https://github.com/obsidiansystems/obelisk/pull/589))
* Fix `ob` commands to again support running from any subdirectory of an obelisk project ([#591](https://github.com/obsidiansystems/obelisk/pull/591))
* Add `reflex-platform-func` argument to Obelisk's `default.nix`. It defaults to it's prior behavior of using the reflex-platform in in `dep`. ([#612](https://github.com/obsidiansystems/obelisk/pull/612))

## v0.3.0.0 - 2019-12-20

* Change the structure of Obelisk routes to use a designated
  `FullRoute` type. This combines frontend and backend routes into one
  structure. This is a **breaking** change which requires Obelisk apps
  to take specific migrations. They are:
    * Rewrite the implementation of `_backend_routeEncoder` in
      `Backend` to use `mkFullRouteEncoder` instead of
      `handleEncoder`. Specifically, the backend and frontend cases of
      the top-level `pathComponentEncoder` become the second and third
      arguments of `mkFullRouteEncoder` respectively, while the
      missing route becomes the first argument. An example of how to
      do this is available in [a reflex-examples
      commit](https://github.com/reflex-frp/reflex-examples/commits/28f566c3e7dc615578dc74297b7c620c1f13683e).
    * Replace type constructions of `InL` with `FullRoute_Backend` and
      `InR` with `FullRoute_Frontend`.
* Generalised `pathSegmentEncoder`, added `pathFieldEncoder`.
* Added some `Prism`s to the encoder library for manipulating `DSum`s.
* Add `ob doc` command, which lists paths to haddock documentation for specified packages.
* Bump reflex-platform so that obelisk now uses GHC 8.6.5 and the nixos-19.03 nixpkgs set.
* Add support in `obelisk-route` for single parameters in URL paths.
* Bump reflex-platform so that obelisk now uses reflex-dom 0.5.2.0.
* Use a `--pure` nix shell in `ob run` for parity with `ob repl` and more resilience against "works on my machine".
* Pin usages of `<nixpkgs>` in obelisk thunks, etc. to the nixpkgs used by the project's obelisk impl.
* Backport ACMEv2 support in obelisk server to regain LetsEncrypt account creation.
* Enable HTTPS in `ob run`.
* `ob run` now handles `ghci` errors better, and includes a custom `ghcid`
  version. As a result, you no longer need to have ghcid installed to
  use `ob run`, as we provide one for you.
* `ob` commands now complain less on systems with umasks other than `0022`.
* Ignore package environment files in `ob run` and `ob repl`.
* Add `Obelisk.Route.Frontend.routeLinkDynAttr`.

## v0.2.0.0 - 2019-8-17

* Configs become ByteStrings.
* FrontendConfigsT has been changed into ConfigsT and configs are made available via getConfig/getConfigs
  * The frontend will still only have access to configs that are placed in config/frontend and config/common, while the backend has access to the entire contents of the config directory via `Obelisk.ExecutableConfig.Lookup.getConfigs`.
* The backend no longer runs in BackendConfigsT.
* Add tabulation package. See Data.Tabulation for details.
* Add encoders for `DMap`, `HasFields` (cf. Data.Tabulation), and JSON.
* Use IP address for nginx proxy pass instead of localhost

## v0.1.1.0 - 2019-05-17

* Fix crashes of Android apps on 32-bit ARM devices.
* Provide a way to indicate acceptance of the Android SDK license by passing `config.android_sdk.accept_license = true;` in the arguments to the import of `.obelisk/impl` in the project's `default.nix`.
* Add `COMPLETE` pragma to `(:/)`. Using this pattern synonym should no longer generate spurious warnings about non-exhaustive pattern matching.
* Make asset path hashing strict (see `Obelisk.Asset.Gather`)
* Add the `ob shell` command to enter a nix shell for an obelisk project
* Allow skeleton's obelisk to be overridden. This changes the skeleton's default.nix interface: the arguments that it used to take are now part of the new "obelisk" argument.
* Removed `MonadIO` from `ObeliskWidget` to prevent accidental IO during prerendering. If you need to do IO in a widget it should be on the right hand side of a `prerender`.
* Significantly changed the interface to the "executable config" packages. `obelisk-executable-config-lookup` is a new internal package which looks up all configs in a platform-specific way. `obelisk-executable-frontend` and `obelisk-executable-backend` provide MTL-style monad classes (`HasFrontendConfigs` and `HasBackendConfigs`) which the frontend and backend, respectively, can use to look up configs. This replaces the old `get` function which ran in `IO`.
* Add a flag to force thunk packing even if there are unpushed changes in the unpacked thunk.


## v0.1.0.0 - 2019-03-29

* Use reflex-dom's `HydrationDomBuilder` to "hydrate" statically rendered DOM served by the Obelisk backend (rather than re-creating DOM and replacing it all).
* Add `HasCookies` and `CookiesT` to allow `ObeliskWidget`s to access cookies during static and "hydrated" DOM rendering.

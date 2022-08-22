# Revision history for obelisk

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released.

## Unreleased

* Documentation
  * [#919](https://github.com/obsidiansystems/obelisk/pull/919): Document useful command for testing obelisk branches to CONTRIBUTING.md
  * [#913](https://github.com/obsidiansystems/obelisk/pull/913): Add haddocks to Obelisk.Command.Deploy
  * [#931](https://github.com/obsidiansystems/obelisk/pull/931): For `ob deploy init`, command-line option `--check-known-host` corrected in readme, caveat added for multiple matching host-keypairs.
* building
  * [#956](https://github.com/obsidiansystems/obelisk/pull/956): Squelch closure-compiler warnings. They are not very helpful and can cause issues (see: [closure-compiler#3720](https://github.com/google/closure-compiler/issues/3720))
* nix
  * [#968](https://github.com/obsidiansystems/obelisk/pull/968): Expose parts of the server derivation as subattributes. For example, the un-assetified frontend can be built alone with `nix-build -A exe.frontend`.
  * Remove override of acme module that pinned it to the version in nixpkgs-20.03. This is used for automatic https certificate provisioning.
* CLI
  * [#784](https://github.com/obsidiansystems/obelisk/pull/784): hint for users to take advantage of ob shell --no-interpret option for thunks
  * [#916](https://github.com/obsidiansystems/obelisk/pull/916): Add `check-known-hosts` option in `ob deploy init`.
  * [#870](https://github.com/obsidiansystems/obelisk/pull/870): Host redirection added to `ob deploy`. Readme updated with tutorial for new functionality.
  * [#931](https://github.com/obsidiansystems/obelisk/pull/931): Fix bug in `ob deploy init` where `ssh-keygen` was not found in nix store.
  * [#934](https://github.com/obsidiansystems/obelisk/pull/934)[#936](https://github.com/obsidiansystems/obelisk/pull/936): obelisk now always uses absolute paths when generating `.ghci` files for REPL and IDE support. This is a **BREAKING** change for some old tools that could not handle absolute paths properly. However, due to the behavior of cross-volume relative paths on certain platforms, such as modern MacOS, we cannot guarantee proper operation of obelisk with relative paths.
  * [#948](https://github.com/obsidiansystems/obelisk/pull/948): obelisk now always invokes bash instead of the system-wide shell when it can. Some sub-programs, like ghcid, will still use the system-wide shell, which can cause subtle problems due to impurities.
  * [#949](https://github.com/obsidiansystems/obelisk/pull/949): obelisk now configures its output and error streams to transliterate unsupported to a known-good replacement character.
  * [#951](https://github.com/obsidiansystems/obelisk/pull/951): Add `ob run --port` which allows the user to override the port on which the app is served, rather than always using the port configured in the route.
  * [#974](https://github.com/obsidiansystems/obelisk/pull/951): Warn the user when invalid fields are present in `default-extensions`/`default-language`.
* obelisk-route
  * [#915](https://github.com/obsidiansystems/obelisk/pull/915): Add routeLinkAttr to Obelisk.Route.Frontend. This allows the creation of route links with additional, user-specified attributes.
  * [#918](https://github.com/obsidiansystems/obelisk/pull/918): Add GHC 8.10.7 support for `obelisk-route`
  * [#952](https://github.com/obsidiansystems/obelisk/pull/952): Add a `Semigroupoid` instance for the `Encoder` type, compatible with its existing `Category` instance.
  * [#957](https://github.com/obsidiansystems/obelisk/pull/957): ob deploy will now detect when the target machine needs to be rebooted after deployment and automatically do so. This is necessary, for example, when the kernel has been updated.
* Javascript FFI
  * [#844](https://github.com/obsidiansystems/obelisk/pull/844): Jsaddle FFI example extended in skeleton (example project which is installed by `ob init`). Note the remark on minifier renaming in /skeleton/static/lib.js
  * [#903](https://github.com/obsidiansystems/obelisk/pull/903): Added support for a file which allows users to specify global variables and namespaces in JS, that should not be used by the Google Closure Compiler during minification of the GHCJS produced JS. See the [FAQ](FAQ.md).
* Static Assets
  * [#922](https://github.com/obsidiansystems/obelisk/pull/922): Serve .wasm files with the correct MIME type
  * [#930](https://github.com/obsidiansystems/obelisk/pull/930): Add an error to ob run when `static` is called with a path to a file that doesn't exist
  * [#940](https://github.com/obsidiansystems/obelisk/pull/940): Instant, auto update to new configs on ob deploy push
  * [#959](https://github.com/obsidiansystems/obelisk/pull/930): Add an error to ob run when `staticFilePath` is called with a path to a file that doesn't exist

## v1.0.0.0 - 2022-01-04

* Update reflex-platform to v0.9.2.0
  * This updated reflex-dom-core to [0.7](https://github.com/reflex-frp/reflex-dom/releases/tag/reflex-dom-core-0.7.0.0), which removes the `js` type parameter from `Prerender` (i.e., `Prerender js t m` becomes `Prerender t m`) and removes `HasJS` and `HasJSContext`. This resulted in changes to the following Obelisk modules:
    * `Obelisk.Configs`: `HasJSContext` and `HasJS` are no longer derived.
    * `Obelisk.Frontend`: `ObeliskWidget js t route m` no longer has the `js` type parameter. It is now `ObeliskWidget t route m`.
    * `Obelisk.Route.Frontend`: There are no longer `HasJSContext` or `HasJS` instances for `RoutedT`, `SetRouteT`, `RouteToUrlT`.
    * Various functions that were constrained to `Prerender js t m` have been updated to with the constraint `Prerender t m`.

## v0.9.4.0 - 2021-12-30

* Update reflex-platform to v0.9.0.0

## v0.9.3.0 - 2021-12-30

* Update reflex-platform to v0.8.0.3

## v0.9.2.1 - 2021-12-28

* Update reflex-platform to v0.7.2.0

## v0.9.2.0 - 2021-12-28

* Update reflex-platform to v0.7.1.0
* Fix bug [#790](https://github.com/obsidiansystems/obelisk/issues/790) which prevented CSS file loading on ios
* Use TemplateHaskell to determine asset file paths
  * Migration: All uses of `static @"some/path"` become `$(static "some/path")`. Instead of requiring `TypeApplications` and `DataKinds`, modules calling `static` must now enable `TemplateHaskell`.
  * Deprecation: Deprecate static asset modules generated via 'obelisk-asset-manifest-generate' in favor of modules generated via 'obelisk-asset-th-generate'. The new executable takes the same arguments as the old and should be a drop-in replacement. To preserve the old behavior, set `__deprecated.useObeliskAssetManifestGenerate = true;` in your obelisk project configuration.
  * Feature: Files added to the static directory while `ob run` is active no longer require `ob run` to be restarted
* Feature: When `staticFiles` is a derivation, as opposed to a regular directory, produce a symlink to the result of that derivation at `static.out` and have `ob run` serve static assets from that symlink. This makes it possible for the static asset derivation to be rebuilt and the new results served without restarting `ob run`.
* Feature: Rebuild static asset derivations while `ob run` is active as long as the change to the derivation is within the project folder. `ob run` now displays a message ("Static assets rebuilt and symlinked to static.out") whenever static assets have been rebuilt and the new static assets are being served.
* Feature: Add `staticFilePath` to `Obelisk.Generated.Static`. Like `static`, this uses TH to generate a reference to a file. Unlike `static`, this `staticFilePath` generates a path on the filesystem instead of URL path.
* Docs: Added [documentation](lib/route/docs/introduction.md) for obelisk-routes.

## v0.9.1.0

* [#801](https://github.com/obsidiansystems/obelisk/pull/801): Remove errors and warning for local packages without a library component
* [#812](https://github.com/obsidiansystems/obelisk/pull/812): Add support for `NoImplicitPrelude` and other extensions disabled via `No`
* Pinned version bumps:
  * reflex-platform [0.7.0.0](https://github.com/reflex-frp/reflex-platform/releases/tag/v0.7.0.0)
  * hnix 0.8.0
* [#787](https://github.com/obsidiansystems/obelisk/pull/787): Set `immutable` cache control directive when serving content-addressed static assets
* Use iOS SDK 13.2


## v0.9.0.1

* ([#810](https://github.com/obsidiansystems/obelisk/pull/810)) Fix loading of `all.js` in fully compiled web apps.

## v0.9.0.0

* **(Breaking change)** Backport nixpkgs upgrades to ACME/Let's Encrypt handling so that HTTPS deployments continue to work flawlessly. If your deployment is having trouble renewing [Let's Encrypt](https://letsencrypt.org/) certificates, upgrade to this version.
  * **IMPORTANT:** In order to use [Let's Encrypt](https://letsencrypt.org/) you must now accept their [terms of service](https://letsencrypt.org/repository/). To do that, add `terms.security.acme.acceptTerms = true;` to the `import ./.obelisk/impl {` section in your `default.nix`. The new skeleton application may serve as an [example](https://github.com/obsidiansystems/obelisk/blob/4759342ab3570888c027d4c58cb5694cb832d624/skeleton/default.nix#L13).
* Update reflex-platform dependency to v0.6.0.0
* ([#715](https://github.com/obsidiansystems/obelisk/pull/715)) In `Obelisk.Route` deprecate `isoEncoder` and `prismEncoder` in favor of more precisely named `viewEncoder` and `reviewEncoder` (respectively) and improve documentation regarding contravariance of `reviewEncoder`.
* ([#739](https://github.com/obsidiansystems/obelisk/pull/739)) Improve `ob shell` by allowing commands to be passed verbatim after a `--` argument. For example, `ob shell 'run command'` can now be written `ob shell -- run command`.
* ([#735](https://github.com/obsidiansystems/obelisk/pull/735)) Fix regression causing custom `Prelude`s to break `ob run`/`ob watch`/`ob repl`.
* ([#737](https://github.com/obsidiansystems/obelisk/pull/737)) Fix bug causing custom `Prelude`s to break `ob profile`.
* ([#752](https://github.com/obsidiansystems/obelisk/pull/752)) Fix the `ob` command-line tool to return non-zero exit code when underlying processes fail.
* ([#742](https://github.com/obsidiansystems/obelisk/pull/742), [#57](https://github.com/obsidiansystems/obelisk/pull/57), [#406](https://github.com/obsidiansystems/obelisk/pull/406)) Enable `-dedupe` and `-DGHCJS_BROWSER` in GHCJS builds to make JavaScript output considerably smaller leading to faster load/parse times and faster build times.
  * **Migration:** New Obelisk projects will automatically benefit from this change, but existing projects need to apply a change similar to [this one](https://github.com/obsidiansystems/obelisk/blob/371cb3302085601c5ec73e9574d51c8b95e3e493/skeleton/frontend/frontend.cabal#L32-L34).
* ([#742](https://github.com/obsidiansystems/obelisk/pull/742)) Update `reflex-platform` which includes:
    * A new version of GHCJS where `-dedupe` is fixed.

## v0.8.0.0

* ([#674](https://github.com/obsidiansystems/obelisk/pull/674), [#711](https://github.com/obsidiansystems/obelisk/pull/711)) Introduce a new thunk format to support accessing the thunk's source directly when packed. When packed, thunks have an additional file called `thunk.nix` and `default.nix` is now a thin wrapper around that.
* ([#665](https://github.com/obsidiansystems/obelisk/pull/665)) Add `--interpret` and `--no-interpret` options to `ob run`/`ob watch`/`ob repl`/`ob shell`. These options allow you to pick which paths will be pre-compiled by `nix` when entering the shell/session and which won't. For example `ob run --no-interpret dep` will ensure that any dependencies found in `./dep` will be built by `nix` before loading the rest of the project into the `ghci` session. The same configuration for `ob shell` will ensure that those packages are built and available in the `ghc` package database inside the shell.

  **NOTE:** `ob shell`'s default behavior is now different. By default it now behaves like `ob run`/`ob watch`/`ob repl` in that it does *not* pre-build any packages whose `.cabal` or `package.yaml` files are found in the project. To regain the previous behavior, use `ob shell --no-interpret . --interpret backend --interpret common --interpret frontend` from the project root.
* ([#695](https://github.com/obsidiansystems/obelisk/pull/695)) `ob deploy init` now requires that your obelisk project be a clean `git` checkout with pushed changes. It has always required that your obelisk project be a `git` repository, but it did not require that your local changes be committed and pushed. This new requirement is added to ensure users don't accidentally create a deployment pointing to an old version of the project.
* ([#705](https://github.com/obsidiansystems/obelisk/pull/705)) Add `Obelisk.Route.packTextEncoder` and generalize `Obelisk.Route.unpackTextEncoder` over any `Data.Text.Lens.IsText`.
* ([#712](https://github.com/obsidiansystems/obelisk/pull/712)) Update [`reflex-platform`](https://github.com/reflex-frp/reflex-platform) to version 0.5.3.0.
* ([#700](https://github.com/obsidiansystems/obelisk/pull/700)) Ensure `ob init` uses the thunk format of the target obelisk rather than the one currently running the init command. If a user had installed a version of obelisk with a new thunk format, a newly initialized skeleton of an older version would not be able to unpack its own `.obelisk/impl`.
* ([#693](https://github.com/obsidiansystems/obelisk/pull/693)) Fix a bug where some packages in `.attr-cache` directories would be incorrectly picked up and used instead of the correct ones when using `ob run`/`ob watch`/`ob repl`.
* ([#709](https://github.com/obsidiansystems/obelisk/pull/709)) Fix a bug in obelisk's preprocessor causing it to incorrectly skip files in some circumstances.
* ([#663](https://github.com/obsidiansystems/obelisk/pull/663)) Add **experimental** support for [`ghcide`](https://github.com/digital-asset/ghcide). See the README for more information.
* ([#714](https://github.com/obsidiansystems/obelisk/pull/714)) Miscellaneous improvements to CLI help and logging.

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
* Add `reflex-platform-func` argument to Obelisk's `default.nix`. It defaults to its prior behavior of using the reflex-platform in in `dep`. ([#612](https://github.com/obsidiansystems/obelisk/pull/612))

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

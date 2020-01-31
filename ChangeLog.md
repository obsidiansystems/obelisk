# Revision history for obelisk

This project's release branch is `master`. This log is written from the perspective of the release branch: when changes hit `master`, they are considered released, and the date should reflect that release.

## Unreleased

* Add `Obelisk.Route.(?/)`, a convenience function for constructing routes nested in `Maybe`. ([#457](https://github.com/obsidiansystems/obelisk/pull/457))
* Add local unpacked packages to the `ob run`, `ob watch`, and `ob repl` sessions. Any `.cabal` or hpack package inside the current obelisk project will be loaded into the session. For `ob run`/`ob watch` this means the session will automatically reload when you save a source file in any of those packages. For `ob repl` it means that `:r` will reload changes to any of those packages. There are some edge cases where this integration is still rough. Report any issues you encounter. ([#489](https://github.com/obsidiansystems/obelisk/pull/489))
* Add `ob hoogle` command to start a local [Hoogle](https://hoogle.haskell.org/) server for the project. ([#628](https://github.com/obsidiansystems/obelisk/pull/628))

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

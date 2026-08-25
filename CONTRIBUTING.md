# Contribution Guide <!-- omit in toc -->

Contributions and issue reports are encouraged and appreciated!

- [Opening Issues](#opening-issues)
- [Submitting Changes](#submitting-changes)
  - [Guidelines for Commit Messages](#guidelines-for-commit-messages)
    - [Summary Line](#summary-line)
      - [Note on bumping dependencies](#note-on-bumping-dependencies)
    - [Body](#body)
  - [Guidelines for Pull Requests](#guidelines-for-pull-requests)
  - [Code Quality](#code-quality)
    - [Warnings](#warnings)
    - [Build and Test](#build-and-test)
  - [Documentation](#documentation)
    - [In the code](#in-the-code)
    - [In the Changelog](#in-the-changelog)
    - [In the Readme](#in-the-readme)
- [Development Environment](#development-environment)
  - [Working on obelisk libraries](#working-on-obelisk-libraries)
  - [Testing with the skeleton](#testing-with-the-skeleton)

## Opening Issues

Before opening an issue, please check whether your issue has already been reported. Assuming it has not:

* Describe the issue you're encountering or the suggestion you're making
* Include any relevant steps to reproduce or code samples you can. It's always easier for us to debug if we have something that demonstrates the error.
* Let us know what version of this project you were using. If you're using a github checkout, provide the git hash.

## Submitting Changes

Most pull requests should target the `develop` branch. `master` is the release branch. `develop` is periodically merged into master after a period of testing.

### Guidelines for Commit Messages

#### Summary Line
The summary line of your commit message should summarize the changes being made. Commit messages should be written in the imperative mood and should describe what happens when the commit is applied. If your commit modifies one of the in-tree haskell packages (found in `./lib`), please prefix your commit summary with the name of the package being modified.

One way to think about it is that your commit message should be able to complete the sentence:
"When applied, this commit will..."

##### Note on bumping dependencies
Commits that update a dependency should include some information about why the dependency was updated in the commit message.

#### Body
For breaking changes, new features, refactors, or other major changes, the body of the commit message should describe the motivation behind the change in greater detail and may include references to the issue tracker. The body shouldn't repeat code/comments from the diff.

### Guidelines for Pull Requests

Wherever possible, pull requests should add a single feature or fix a single bug. Pull requests should not bundle several unrelated changes.


### Code Quality

#### Warnings

Your pull request should add no new warnings to the project. It should also generally not disable any warnings.

#### Build and Test

Make sure the project builds and that the tests pass! This will generally also be checked by CI before merge, but trying it yourself first means you'll catch problems earlier and your contribution can be merged that much sooner!

Build and test the libraries:
```bash
cd lib && cabal build all
cd lib && cabal test all
```

Build the skeleton with both drivers and frontend targets:
```bash
nix-build release.nix -A all
```

Or build individual targets:
```bash
nix-build skeleton -A haskell-nix.serverExe.wasm
nix-build skeleton -A haskell-nix.serverExe.js
nix-build skeleton -A nixpkgs.serverExe.js
nix-build skeleton -A nixpkgs.serverExe.wasm
```

Or one cell of the release matrix:
```bash
nix-build release.nix -A serverExe.haskell-nix.wasm
nix-build release.nix -A shell-build.nixpkgs.wasm
```


### Documentation

#### In the code
We're always striving to improve documentation. Please include [haddock](https://haskell-haddock.readthedocs.io/en/latest/index.html) documentation for any added code, and update the documentation for any code you modify.

#### In the [Changelog](ChangeLog.md)
Add an entry to the changelog when your PR:
* Adds a feature
* Deprecates something
* Includes a breaking change
* Makes any other change that will impact users

#### In the [Readme](README.md)
The readme is the first place a lot of people look for information about the repository. Update any parts of the readme that are affected by your PR.

## Development Environment

### Working on obelisk libraries

Enter the skeleton's nix shell to get a development environment with all obelisk libraries available:

```bash
cd skeleton
nix-shell -A haskell-nix  # or: nix develop
```

From within the shell, uncomment the obelisk `optional-packages` stanzas in `cabal.project` to develop obelisk libraries alongside the skeleton:

```cabal
optional-packages:
  deps/obelisk/lib/*
  deps/obelisk/lib/*/*
```

Then use `ob-run` for live feedback, or `ob-repl` for a REPL:

```bash
ob-run       # watch-and-rebuild development server
ob-repl      # REPL with optimizations disabled
```

### Testing with the skeleton

The skeleton serves as the integration test for obelisk. To verify your changes work end-to-end:

```bash
cd skeleton
nix-shell -A haskell-nix
ob-run       # test development workflow
```

For a full production build test:
```bash
nix-build release.nix -A all
```

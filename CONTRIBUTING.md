# Contribution Guide

Contributions and issue reports are encouraged and appreciated!

- [Opening Issues](#opening-issues)
- [Submitting Changes](#submitting-changes)
  - [Guidelines for Commit Messages](#guidelines-for-commit-messages)
  - [Code Quality](#code-quality)
  - [Documentation](#documentation)

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

### Code Quality

#### Warnings

Your pull request should add no new warnings to the project. It should also generally not disable any warnings.

#### Build and Test

Make sure the project builds and that the tests pass! This will generally also be checked by CI before merge, but trying it yourself first means you'll catch problems earlier and your contribution can be merged that much sooner!

You can run the tests like this:
```bash
$(nix-build -A selftest --no-out-link)
```

To test that your changes build across platforms, you can also try to build release.nix, like this:
```bash
nix-build release.nix
```

Note, however, that to build release.nix you must accept the android license agreement and your machine must be configured to build both ios and android executables (usually via remote builders).


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


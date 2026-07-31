# Machine setup: Nix, binary caches, private repositories

What a machine needs before it can build an Obelisk project. One-time setup;
after this, the [Quick Start](../README.md#quick-start) applies.

## 1. Install Nix

Install [Nix](https://nixos.org/download/) (multi-user install recommended).
Any Nix 2.x works for `nix-shell`/`nix-build`; the flake commands
(`nix develop`, `nix build`) also need `experimental-features = nix-command
flakes` in your Nix configuration.

Note: obelisk's nix builds currently support Linux only (`x86_64-linux` and
`aarch64-linux`).

Note: obelisk's own dependencies are git submodules of the obelisk repo.
Nix 2.27+ fetches them automatically (obelisk's flake declares
`inputs.self.submodules = true`); on older Nix, add `?submodules=1` to the
obelisk flake URL, and clone checkouts with `--recurse-submodules`.

## 2. Set up binary caches

Without the caches below, your first build compiles the GHC 9.14 toolchain
and the WASM cross-compiler from source, which takes hours. With them it is a
download. If any early build step runs longer than a few minutes, the caches
are probably not active.

The project's `flake.nix` declares the caches in `nixConfig`, so `nix
develop`/`nix build` will offer to use them, but only if your user is in
`trusted-users`, and classic `nix-shell`/`nix-build` ignores `nixConfig`
entirely. Configuring them globally covers every case.

On NixOS, add to `configuration.nix` and rebuild:

```nix
nix.settings = {
  experimental-features = [ "nix-command" "flakes" ];
  substituters = [
    "https://cache.nixos.org"
    "https://nixcache.reflex-frp.org"
    "https://cache.iog.io"
  ];
  trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];
};
```

On other Linux distributions, put the same values in `/etc/nix/nix.conf`:

```
experimental-features = nix-command flakes
substituters = https://cache.nixos.org https://nixcache.reflex-frp.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
```

then restart the daemon: `sudo systemctl restart nix-daemon`.

`nixcache.reflex-frp.org` serves the reflex/obelisk toolchain;
`cache.iog.io` serves the haskell.nix infrastructure that nix-haskell builds
on.

## 3. Accessing private repositories

If your project pins dependencies from private git repositories (as
`source-repository-package` stanzas in `cabal.project`, or as nix-thunks
under `deps/` consumed via `obeliskLib.thunkSource`), the Nix builder fetches them over
SSH, so the building user needs SSH access to the host:

- [GitHub](https://docs.github.com/en/authentication/connecting-to-github-with-ssh)
- [GitLab](https://docs.gitlab.com/ee/user/ssh.html)

With a multi-user Nix install, fetches run as your user for
`builtins.fetchGit`-style fetchers, but fixed-output derivations run in the
sandbox; if a private fetch fails there, use an SSH agent and
`ssh-ng://`-style access, or vendor the dependency under `deps/`.

# Obelisk.ExecutableConfig

## About

Obelisk.ExecutableConfig serves two related purposes:

1. Injection: a cross-platform injection function, which can be used to put configuration files in a canonical location.
2. Retrieval: a cross-platform retrieval function that is aware of the canonical location on each platform and how to read files from that location.

## File Location

ExecutableConfig expects your configuration files to be placed in `./exe-config`.

## Supported Platforms

| Function | iOS | Android | Warp |Web backend + GHCJS |
|----------|:---:|:-------:|:----:|:------------------:|
| inject   | ✔   | ✔       |      | ✔                  |
| get      | ✔   | ✔       | ✔    | ✔                  |

### iOS and Android

`inject` is a Nix function that copies the specified configuration folder to a canonical location on the device.

`get` is a Haskell function that retrieves configuration files from the canonical location.

### Warp

There is currently no `inject` function for `jsaddle-warp`-based frontends.

`get` is a Haskell function that simply reads the configuration files from `./exe-config`.

### Web backend + GHCJS

#### Backend

`inject` is a Haskell function that produces a snippet of HTML that must be appended to the `<head>` of the page that the backend serves (i.e., the entry point for the GHCJS application).

`get` is a Haskell function that reads the configuration files from `./exe-config`.

#### GHCJS

`get` is a Haskell function that can be run by the GHCJS frontend to retrieve the injected configuration data from the served `<head>`. There is no `inject` that is run in GHCJS.


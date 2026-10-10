# Obelisk.ExecutableConfig

## About

Obelisk.ExecutableConfig serves two related purposes:

1. Injection: a cross-platform injection function, which can be used to put configuration files in a canonical location.
2. Retrieval: a cross-platform retrieval function that is aware of the canonical location on each platform and how to read files from that location.

## Supported Platforms

The lookup library (`obelisk-executable-config-lookup`) provides two implementations of `get`, selected at build time:

- **Native / other** (`src-other`, used for the backend and `jsaddle-warp`): reads the configuration files from the `config` directory relative to the current directory.
- **Frontend** (`src-js`, used for the cross-compiled `javascript`/`ghcjs`/`wasm32` frontend): retrieves the injected configuration data from the served `<head>`.

| Function | Native backend / Warp | Cross-compiled frontend (JS/GHCJS/WASM) |
|----------|:---------------------:|:---------------------------------------:|
| inject   | x                     |                                         |
| get      | x                     | x                                       |

### Native backend / Warp

`inject` is a Haskell function that produces a snippet of HTML that must be appended to the `<head>` of the page that the backend serves (i.e., the entry point for the frontend application).

`get` is a Haskell function that reads the configuration files from the `config` directory relative to the current directory.

### Cross-compiled frontend (JS/GHCJS/WASM)

`get` is a Haskell function that runs in the frontend to retrieve the injected configuration data from the served `<head>`. There is no `inject` that runs in the frontend.

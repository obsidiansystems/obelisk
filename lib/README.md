# Obelisk libraries

This directory contains libraries provided by obelisk. By convention they are prefixed with `obelisk-` and their module exports all lie within the `Obelisk` import namespace. For example, to include `frontend` as a dependency you would add `obelisk-frontend` to your cabal dependencies, and import `Obelisk.Frontend...` in the source files.

## Local Development with Haskell Language Server

To use Haskell Language Server on the libraries in this directory, you are required to set up some HLS specific files, via:
```sh
./setupHls.sh up
```

Once you are done with development, you may tear down the environment via:
```sh
./setupHls.sh down
```

#!/usr/bin/env nix-shell
#! nix-shell -p bash -i bash
# shellcheck shell=bash

CMD=$1
shift

libRoot=$(dirname "$0")

case $CMD in
  up)
    cp "$libRoot/cabal.project.dev" "$libRoot/cabal.project"
    ;;
  down)
    rm "$libRoot/cabal.project"
    ;;
  *)
    echo "SYNOPSIS: $0 (up|down)"
    echo "  up:   Set up dev environment suitable for HLS"
    echo "  down: Remove HLS specific files"
esac

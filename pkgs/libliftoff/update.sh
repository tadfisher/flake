#!/usr/bin/env bash

set -eu -o pipefail

root="$(git rev-parse --show-toplevel)"
pkg="$(dirname $(realpath $0))"

cd $root

nix run nixpkgs#nix-prefetch-github -- emersion libliftoff --no-fetch-submodules > "${pkg}/source.json"
nix run nixpkgs#curl -- -s "https://api.github.com/repos/emersion/libliftoff/commits/master" \
    | nix run nixpkgs#jq -- -r '.commit.committer.date' \
    | head -c 10 > "${pkg}/version"

cd -

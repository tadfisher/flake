#!/usr/bin/env bash

set -eu -o pipefail

root="$(git rev-parse --show-toplevel)"
pkg="$(dirname $(realpath $0))"
branch="develop"

cd $root

nix run nixpkgs#nix-prefetch-github -- paperwm PaperWM --rev "$branch" > "${pkg}/source.json"
nix run nixpkgs#curl -- -s "https://api.github.com/repos/paperwm/PaperWM/commits/${branch}" \
    | nix run nixpkgs#jq -- -r '.commit.committer.date' \
    | head -c 10 > "${pkg}/version"

cd -

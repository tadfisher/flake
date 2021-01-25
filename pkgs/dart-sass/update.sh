#!/usr/bin/env bash

set -eu -o pipefail

root="$(git rev-parse --show-toplevel)"
pkg="$(realpath "$(dirname $0)")"
version="$(git ls-remote --tags --sort=-v:refname 'git@github.com:sass/dart-sass.git' | head -n 1 | grep -oP 'refs/tags/\K(.*)')"
echo $version
url="https://github.com/sass/dart-sass/archive/${version}.tar.gz"
echo $url
hash="$(nix-prefetch-url -- --unpack "$url")"
echo $hash

jq --arg version "$version" \
   --arg url "$url" \
   --arg hash "$hash" \
   -n '$ARGS.named' > "${pkg}/source.json"

tmp="$(mktemp -d)"
curl -sL "$url" | tar -xz -C "$tmp"
cd "${tmp}/dart-sass-${version}"
nix shell ${root}#dart -c pub get
nix run ${root}#pub2nix-lock
cd -

cp "${tmp}/dart-sass-${version}/pub2nix.lock" "${pkg}/pub2nix.lock"

#!/usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils curl git gnused jq nixFlakes

shopt -s extglob
set -eu -o pipefail

root="$(git rev-parse --show-toplevel)"
path="$(dirname "$0")"

pushd "$root" > /dev/null

oldVersion="$(nix eval --raw .#unifi-beta.version)"
oldHash="$(nix eval --raw .#unifi-beta.src.drvAttrs.outputHash)"

version="$(curl -s "https://repology.org/api/v1/project/unifi" | jq -r 'map(select(.status == "newest")) | .[0].version')"
source="https://dl.ui.com/unifi/$version/unifi_sysvinit_all.deb"
hash="$(nix-prefetch-url --type sha256 "$source" 2>&1 | tail -1)"

sed -i "$path/default.nix" -re "s|\"$oldVersion\"|\"$version\"|"
sed -i "$path/default.nix" -re "s|\"$oldHash\"|\"$hash\"|"

popd > /dev/null

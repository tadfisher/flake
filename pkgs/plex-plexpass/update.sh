#!/usr/bin/env nix-shell
#! nix-shell -i bash -p coreutils curl git gnused jq nixFlakes

shopt -s extglob
set -eu -o pipefail

root="$(git rev-parse --show-toplevel)"
path="$(dirname "$0")"

pushd "$root" > /dev/null

oldSource="$(nix eval --raw .#plexRaw-plexpass.src.urls --apply builtins.head)"
oldVersion="$(nix eval --raw .#plexRaw-plexpass.version)"
oldHash="$(nix eval --raw .#plexRaw-plexpass.src.drvAttrs.outputHash)"

token=$(cat "$root/secrets/kepler/plex-token")
manifest=$(curl -s "https://plex.tv/api/downloads/5.json?channel=plexpass" -H "X-Plex-Token: ${token}")
version=$(echo "$manifest" | jq -r '.computer.Linux.version | split("-") | .[0]')
sed -i "$path/raw.nix" -re "s|\"$oldVersion\"|\"$version\"|"

for arch in "linux-x86_64" "linux-aarch64"; do
  source="$(echo "$manifest" | jq --arg arch "$arch" -r '.computer.Linux.releases[] | select(.distro == "debian" and .build == $arch) .url')"
  hash="$(nix-prefetch-url --type sha256 "$source" 2>&1 | tail -1)"
  sed -i "$path/raw.nix" -re "s|\"$oldSource\"|\"$source\"|"
  sed -i "$path/raw.nix" -re "s|\"$oldHash\"|\"$hash\"|"
done

popd > /dev/null

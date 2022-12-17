#!/usr/bin/env nix-shell
#!nix-shell -i bash -p curl findutils jq

set -o errexit
set -o nounset
set -o pipefail

pushd $(dirname -- "$0") >/dev/null

curl 'https://data.services.jetbrains.com/products?code=TBA&release.type=release' |
    jq '.[0].releases[0] | {downloads, build}' > releases.tmp

jq -rj '.build' releases.tmp > version

jq -r '
def nixPlatform: . as $s | {
    linux: "x86_64-linux",
    windows: "x86_64-windows",
    mac: "x86_64-darwin",
    macM1: "aarch64-darwin"
  } | .[$s]?
;

.downloads
  | to_entries
  | map(.key |= nixPlatform)
  | map(.value |= {link, checksumLink})
  | .[]
  | select(.key != null)
  | { platform: .key, url: .value.link, hash: .value.checksumLink }
' releases.tmp > sources.tmp

jq -rs '.[].hash' sources.tmp.json \
  | xargs curl -s \
  | cut -d ' ' -f 1 \
  | xargs nix hash to-sri --type sha256 > hashes.tmp

jq -s --rawfile hashes hashes.tmp '
[$hashes | split("\n") | .[] | select(. != "")] as $hashes
  | [., $hashes]
  | transpose
  | map(.[0] + { hash: .[1]})
  | INDEX(.platform)
  | map_values({url, hash})
' sources.tmp > sources.json

rm *.tmp

popd >/dev/null

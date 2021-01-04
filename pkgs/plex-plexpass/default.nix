{ stdenv
, plex
, plexRaw-plexpass
, fetchurl
, writeScript
, curl
, jq
, common-updater-scripts
}:

plex.override {
  plexRaw = plexRaw-plexpass;
}

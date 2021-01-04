{ stdenv
, plexRaw
, fetchurl
, writeScript
, curl
, jq
, common-updater-scripts
}:

plexRaw.overrideAttrs (attrs: rec {
  pname = attrs.pname + "-plexpass";
  version = "1.21.1.3830";
  src = fetchurl {
    url = "https://downloads.plex.tv/plex-media-server-new/1.21.1.3830-6c22540d5/debian/plexmediaserver_1.21.1.3830-6c22540d5_amd64.deb";
    sha256 = "0hh9bkbkha0c91kizv8w6wm5l64z303jz7r961w8bpzv93sk3b0m";
  };
})

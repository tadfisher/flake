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
    url = "https://downloads.plex.tv/plex-media-server-new/1.21.1.3830-6c22540d5/debian/plexmediaserver_1.21.1.3830-6c22540d5_arm64.deb";
    sha256 = "12lpf7j89ygrxhpn93llgmf24ilq3w8fdqpg4hr7zil4p274fs15";
  };
})

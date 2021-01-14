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
  version = "1.21.1.3876";
  src = fetchurl {
    url = "https://downloads.plex.tv/plex-media-server-new/1.21.1.3876-3c3adfcb4/debian/plexmediaserver_1.21.1.3876-3c3adfcb4_arm64.deb";
    sha256 = "1xpsmk5l0f0blqp5ba9n1w0npsk692p07hp4ipkq7yz3mfag50p0";
  };
})

{ unifi
, fetchurl
}:

unifi.overrideAttrs (attrs: rec {
  version = "6.1.26";
  src = fetchurl {
    url = "https://dl.ui.com/unifi/${version}/unifi_sysvinit_all.deb";
    sha256 = "1zfwlrf752vy9x6yp95rn6xcsb332rbwprxpdw4ivdlabxkziah7";
  };
})

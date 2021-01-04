{ stdenv
, fetchFromGitLab
, autoreconfHook
, autoconf-archive
, bubblewrap
, chrpath
, docbook5
, docbook-xsl-nons
, docbook-xsl-ns
, elfutils
, gtk-doc
, perlPackages
, pkgconfig
, glib
, libxml2
, libxslt
, zlib
, autoconf
, gnused
, utillinux
}:
let
  host = "${stdenv.system}-gnu";

in
stdenv.mkDerivation rec {
  pname = "libcapsule";
  version = "0.20201120.0";

  src = fetchFromGitLab {
    domain = "gitlab.collabora.com";
    owner = "vivek";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-iZG5O04DIKPvLYSQ/7PnyMlm8BFf9PlwhV1Nr6P4tCY=";
  };

  nativeBuildInputs = [
    autoreconfHook
    autoconf-archive
    bubblewrap
    chrpath
    docbook5
    docbook-xsl-nons
    docbook-xsl-ns
    elfutils
    gtk-doc
    perlPackages.perl
    perlPackages.IPCRun
    pkgconfig
    glib
    libxml2
    libxslt
    zlib
  ];

  buildInputs = [
    autoconf
    gnused
    pkgconfig
    utillinux
  ];

  outputs = [ "out" "dev" "doc" ];

  configureFlags = [
    "--host=${host}"
    "--disable-gtk-doc"
  ];

  patchPhase = ''
    patchShebangs data/capsule-init-project
    patchShebangs data/capsule-mkstublib
  '';

  preAutoreconf = ''
    gtkdocize
  '';

  meta = with stdenv.lib; {
    description = "Segregated dynamic linking library";
    homepage = "https://gitlab.collabora.com/vivek/libcapsule";
    platforms = [ "x86_64-linux" "i686-linux" ];
    license = licenses.lgpl21Plus;
    maintainers = [ maintainers.tadfisher ];
  };
}

{ lib
, stdenv
, src
, keyutils
}:

stdenv.mkDerivation {
  pname = "sedcli";
  version = "unstable";

  inherit src;

  buildInputs = [ keyutils ];

  postUnpack = ''
    sourceRoot=$sourceRoot/src
  '';

  dontAddPrefix = true;

  preConfigure = ''
    patchShebangs configure
    substituteInPlace Makefile \
      --replace "/usr/" "$out/" \
      --replace "/etc/" "$out/etc"
  '';

  makeFlags = [
    "LIB_DIR=$(out)/lib"
  ];

  preInstall = ''
    mkdir -p $out/bin $out/lib $out/sbin $out/share/man/man8
  '';

  meta = with lib; {
    description = "sedcli and libsed library for NVMe Self-Encrypting Drives (SEDs) management";
    homepage = "https://github.com/sedcli/sedcli";
    license = with licenses; [ gpl2Plus lgpl21Plus ];
    platforms = platforms.linux;
  };
}

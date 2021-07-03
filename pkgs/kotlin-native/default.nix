{ lib
, stdenv
, fetchurl
, callPackage
, autoPatchelfHook
, jdk
, ncurses5
, llvmPackages
, zlib
}:

stdenv.mkDerivation rec {
  pname = "kotlin-native";
  version = "1.5.20";

  src = fetchurl {
    url = "https://github.com/JetBrains/kotlin/releases/download/v${version}/kotlin-native-linux-${version}.tar.gz";
    hash = "sha256-lEkhnslGWxSt2htzCsFO8C2pPp+YIZ9zA79wxMh1t9s=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    jdk
    llvmPackages.libclang
    ncurses5
    stdenv.cc.cc.lib
    zlib
  ];

  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';

  passthru = {
    inherit llvmPackages;
  };
}

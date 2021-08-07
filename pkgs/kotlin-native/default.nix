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
  version = "1.5.30-M1";

  src = fetchurl {
    url = "https://github.com/JetBrains/kotlin/releases/download/v${version}/kotlin-native-linux-x86_64-${version}.tar.gz";
    hash = "sha256-iPAhaI02opBiCBJ04YEBssxhm5vdbnJrqGkBDMLuGyc=";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    jdk
    llvmPackages.libclang
    ncurses5
    stdenv.cc.cc.lib
    zlib
  ];

  propagatedBuildInputs = [ jdk ];

  installPhase = ''
    mkdir -p $out
    cp -r * $out
  '';

  passthru = {
    inherit llvmPackages;
  };
}

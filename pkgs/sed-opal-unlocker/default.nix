{ lib
, stdenv
, fetchFromGitHub
, runCommand
, python3
, libargon2
}:
let
  version = "2021-02-12";

  src = fetchFromGitHub {
    owner = "dex6";
    repo = "sed-opal-unlocker";
    rev = "e68060247614be08e2b08e617142ab58405b761e";
    sha256 = "gBMsHYR91kl26qqa2kkyEPhBxwQhXGngIOLD3nVB038=";
  };

  meta = with lib; {
    homepage = "https://github.com/dex6/sed-opal-unlocker";
    license = licenses.asl20;
    maintainers = [ maintainers.tadfisher ];
    platforms = platforms.linux;
  };

  sedutil-passhasher = stdenv.mkDerivation {
    inherit version src;
    name = "sedutil-passhasher";

    buildInputs = [ libargon2 python3 ];

    installPhase = ''
      mkdir -p $out/bin
      cp sedutil-passhasher.py $out/bin/sedutil-passhasher
    '';
  };

  sed-opal-unlocker = stdenv.mkDerivation {
    inherit version src;
    name = "sed-opal-unlocker";

    buildInputs = [ libargon2 ];

    passthru = { inherit sedutil-passhasher; };

    installPhase = ''
      mkdir -p $out/bin
      cp sed-opal-unlocker $out/bin
    '';
  };

in
sed-opal-unlocker

{ lib
, stdenv
, fetchurl
, python3
}:

let
  ath11k-fw-repo = fetchurl {
    url = "https://raw.githubusercontent.com/qca/qca-swiss-army-knife/master/tools/scripts/ath11k/ath11k-fw-repo";
    hash = "sha256-NOoN3uF4Fxq7qsV8cbSB6uVRwKmtNquAM68JDfXNq6Y=";
  };
in
stdenv.mkDerivation {
  pname = "ath11k-firmware";
  version = "unstable-20240813";

  dontUnpack = true;

  nativeBuildInputs = [ python3 ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out/lib/firmware
    python ${ath11k-fw-repo} --install $out/lib/firmware
  '';

  meta = with lib; {
    description = "Firmware for ath11k devices";
    homepage = "https://github.com/kvalo/ath11k-firmware";
    license = licenses.unfreeRedistributableFirmware;
    maintainers = with maintainers; [ tadfisher ];
    platforms = platforms.linux;
  };
}

{ lib
, stdenv
, fetchurl
, python3
}:

let
  ath11k-fw-repo = fetchurl {
    url = "https://raw.githubusercontent.com/qca/qca-swiss-army-knife/master/tools/scripts/ath11k/ath11k-fw-repo";
    hash = "sha256-0xotKi72XhVRrokeerqivqHdU3pPRKjB4qtSMu9VsVQ=";
  };
in
stdenv.mkDerivation {
  pname = "ath11k-firmware";
  version = "unstable-20250630";

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

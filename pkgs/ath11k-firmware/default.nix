{ lib
, stdenv
, src
}:

stdenv.mkDerivation {
  pname = "ath11k-firmware";
  version = "unstable-20240124";
  inherit src;

  dontBuild = true;

  installPhase = ''
    for i in WCN6855/hw2.0/board-2.bin \
             WCN6855/hw2.0/regdb.bin \
             WCN6855/hw2.0/1.1/WLAN.HSP.1.1-03125-QCAHSPSWPL_V1_V2_SILICONZ_LITE-3.6510.37/amss.bin \
             WCN6855/hw2.0/1.1/WLAN.HSP.1.1-03125-QCAHSPSWPL_V1_V2_SILICONZ_LITE-3.6510.37/m3.bin;
    do
      install -D -pm644 $i $out/lib/firmware/ath11k/WCN6855/hw2.0/$(basename $i)
    done

    mkdir -p $out/lib/firmware/ath11k/WCN6855/hw2.1
    ln -sf $out/lib/firmware/ath11k/WCN6855/hw2.0/* $out/lib/firmware/ath11k/WCN6855/hw2.1/
  '';

  meta = with lib; {
    description = "Firmware for ath11k devices";
    homepage = "https://github.com/kvalo/ath11k-firmware";
    license = licenses.unfreeRedistributableFirmware;
    maintainers = with maintainers; [ tadfisher ];
    platforms = platforms.linux;
  };
}

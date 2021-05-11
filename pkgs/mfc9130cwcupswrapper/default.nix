{ stdenv
, lib
, fetchurl
, mfc9130cwlpr
}:

stdenv.mkDerivation rec {
  name = "mfc9130cw-cupswrapper-${version}";
  version = "1.1.4-0";

  src = fetchurl {
    url = "https://download.brother.com/welcome/dlf006786/mfc9130cw_cupswrapper_GPL_source_1.1.4-0.tar.gz";
    sha256 = "01d5nd4xnyg97vybyrad6lm3pc5yn0a4r9cx877dv89gqwp56cxk";
  };

  buildInputs = [ mfc9130cwlpr ];

  patchPhase = ''
    substituteInPlace cupswrapper/cupswrappermfc9130cw \
      --replace /opt "${mfc9130cwlpr}/opt" \
      --replace /usr "${mfc9130cwlpr}/usr" \
      --replace /etc "$out/etc" \
      --replace "\`cp " "\`cp -p " \
      --replace "\`mv " "\`cp -p "
  '';

  buildPhase = ''
    runHook preBuild

    pushd brcupsconfig
    make all
    popd

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    DEST=$out/opt/brother/Printers/mfc9130cw/cupswrapper/
    mkdir -p $DEST

    cp brcupsconfig/brcupsconfpt1 $DEST
    cp cupswrapper/cupswrappermfc9130cw $DEST
    cp PPD/brother_mfc9130cw_printer_en.ppd $DEST

    mkdir -p $out/share/cups/model/Brother
    ln -s $out/opt/brother/Printers/mfc9130cw/cupswrapper/*.ppd \
      $out/share/cups/model/Brother

    runHook postInstall
  '';

  cleanPhase = ''
    pushd brcupsconfig
    make clean
    popd
  '';

  meta = with lib; {
    description = "Brother MFC-9130CW CUPS wrapper driver";
    homepage = "https://www.brother.com/";
    license = licenses.gpl2;
    platforms = platforms.linux;
    downloadPage = "https://support.brother.com/g/b/downloadlist.aspx?c=us&lang=en&prod=mfc9130cw_us&os=128";
    maintainers = [ maintainers.tadfisher ];
  };
}

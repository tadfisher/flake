{ stdenv
, lib
, fetchurl
, cups
, dpkg
, ghostscript
, a2ps
, coreutils
, gnused
, gawk
, file
, makeWrapper
}:

stdenv.mkDerivation rec {
  name = "mfc9130cw-lpr-${version}";
  version = "1.1.2-1";

  src = fetchurl {
    url = "https://download.brother.com/welcome/dlf100410/mfc9130cwlpr-${version}.i386.deb";
    sha256 = "0vbkhpp8mi0c2fpy9f57rly3alcc3mcpgsapfxv3b5yigxvjr8bf";
  };

  nativeBuildInputs = [ makeWrapper ];
  buildInputs = [ cups ghostscript dpkg a2ps ];

  unpackPhase = "true";

  installPhase = ''
    dpkg-deb -x $src $out

    substituteInPlace $out/opt/brother/Printers/mfc9130cw/lpd/filtermfc9130cw \
      --replace /opt "$out/opt"

    sed -i '/GHOST_SCRIPT=/c\GHOST_SCRIPT=gs' $out/opt/brother/Printers/mfc9130cw/lpd/psconvertij2

    patchelf --set-interpreter ${stdenv.cc.libc.out}/lib/ld-linux.so.2 $out/opt/brother/Printers/mfc9130cw/lpd/brmfc9130cwfilter

    mkdir -p $out/lib/cups/filter/
    ln -s $out/opt/brother/Printers/mfc9130cw/lpd/filtermfc9130cw $out/lib/cups/filter/brother_lpdwrapper_mfc9130cw

    wrapProgram $out/opt/brother/Printers/mfc9130cw/lpd/psconvertij2 \
      --prefix PATH ":" ${ lib.makeBinPath [ gnused coreutils gawk ] }

    wrapProgram $out/opt/brother/Printers/mfc9130cw/lpd/filtermfc9130cw \
      --prefix PATH ":" ${ lib.makeBinPath [ ghostscript a2ps file gnused coreutils ] }
  '';

  meta = with lib; {
    homepage = "http://www.brother.com/";
    description = "Brother MFC-9130CW LPR driver";
    license = licenses.unfree;
    platforms = platforms.linux;
    downloadPage = "https://support.brother.com/g/b/downloadlist.aspx?c=us&lang=en&prod=mfc9130cw_us&os=128";
    maintainers = [ maintainers.tadfisher ];
  };
}

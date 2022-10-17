{ stdenv
, lib
, fetchurl
, autoPatchelfHook
, alsa-lib
, freetype
, fontconfig
, glib
, jdk
, libGL
, xorg
, zlib
}:

stdenv.mkDerivation {
  pname = "fleet";
  version = "1.9.231";

  src = fetchurl {
    url = "https://download-cdn.jetbrains.com/fleet/installers/linux_x64/Fleet-1.9.231.tar.gz";
    sha256 = "0kh95f5advqn43gxysbi7flivczbd6n9421d0wjpnrji0q03w8qf";
  };

  nativeBuildInputs = [ autoPatchelfHook ];

  buildInputs = [
    alsa-lib
    fontconfig
    freetype
    glib
    jdk
    libGL
    stdenv.cc.cc.lib
    zlib
  ] ++ (with xorg; [
    libX11
    libXext
    libXi
    libXrender
    libXtst
  ]);

  dontBuild = true;

  installPhase = ''
    mkdir $out
    cp -R * $out
  '';

  meta = with lib; {
    description = "Next-generation IDE by JetBrains";
    homepage = "https://www.jetbrains.com/fleet/";
    maintainers = with maintainers; [ tadfisher ];
    platforms = [ "x86_64-linux" ];
    license = licenses.unfree;
  };
}

{ lib
, stdenv
, fetchurl
, unzip
, autoPatchelfHook
, cmake
, glib
, nss
, nspr
, atk
, at-spi2-atk
, libdrm
, expat
, libxcb
, libxkbcommon
, libX11
, libXcomposite
, libXdamage
, libXext
, libXfixes
, libXrandr
, mesa
, gtk3
, pango
, cairo
, alsa-lib
, dbus
, at-spi2-core
, cups
, libxshmfence
}:

let
  cefTarget = if stdenv.hostPlatform.isx86_64 then "linux64"
    else if stdenv.isAarch64 then "linuxarm64"
    else throw "Unsupported platform: ${stdenv.system}";

  hashes = {
    linux64 = "0nr0z7zh8ndalpnaabpvz698gqvikjgw73pnxbaclcjp8hn15lmx";
    linuxarm64 = "08lwz504r9h93anmvk5yy6yh5a22gm7qd7ji3zj7b2bdv87x2hf0";
  };

  urlescape = lib.replaceStrings [ "+" ] [ "%2B" ];
in
stdenv.mkDerivation rec {
  pname = "cef-binary";
  version = "98.3.34+g97a5ae6+chromium-98.0.4758.102";

  src = fetchurl {
    url = "https://cache-redirector.jetbrains.com/intellij-jbr/cef_binary_${urlescape version}_${cefTarget}_minimal.zip";
    name = "cef_binary_${version}_${cefTarget}_minimal.zip";
    sha256 = hashes.${cefTarget};
  };

  nativeBuildInputs = [ autoPatchelfHook unzip ];

  buildInputs = [
    glib
    nss
    nspr
    atk
    at-spi2-atk
    libdrm
    expat
    libxcb
    libxkbcommon

    libXcomposite
    libXdamage
    libXext
    libXfixes
    libXrandr
    mesa
    gtk3
    pango
    cairo
    alsa-lib
    dbus
    at-spi2-core
    cups
    libxshmfence
  ];

  dontBuild = true;

  installPhase = ''
    mkdir -p $out
    cp -r . $out
  '';

  meta = with lib; {
    description = "Simple framework for embedding Chromium-based browsers in other applications";
    homepage = "https://cef-builds.spotifycdn.com/index.html";
    maintainers = with maintainers; [ puffnfresh ];
    license = licenses.bsd3;
    platforms = [ "x86_64-linux" "aarch64-linux" ];
  };
}

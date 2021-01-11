{ lib
, stdenv
, fetchurl
, meson
, ninja
, bubblewrap
, glslang
, gtk-doc
, elfutils
, glib
, json-glib
, libcapsule
, libcapsule-i686
, libtheora
, libGL
, libudev
, libva
, pandoc
, pkgconfig
, python3
, libvdpau
, vulkan-headers
, vulkan-loader
, vulkan-tools
, waffle
, xorg
, zlib
, steam-runtime-tools-i686 ? null
}:
let
  hostPrefix =
    if steam-runtime-tools-i686 == null
    then "i386-linux-gnu"
    else "x86_64-linux-gnu";

in
stdenv.mkDerivation rec {
  pname = "steam-runtime-tools";
  version = "0.20201203.0";

  src = fetchurl {
    url = "https://gitlab.steamos.cloud/steamrt/steam-runtime-tools/-/archive/v${version}/steam-runtime-tools-v${version}.tar.gz";
    sha256 = "sha256-v25qjgR0yzVspkPgk58jOMW4Q8NyXL0iYgL7JoIX0Ug=";
  };

  nativeBuildInputs = [
    meson
    ninja
    bubblewrap
    glslang
    gtk-doc
    elfutils
    glib
    json-glib
    libtheora
    libGL
    libudev
    libva
    pkgconfig
    python3
    vulkan-headers
    xorg.libX11
    xorg.libXau
    xorg.libxcb
  ];

  buildInputs = [
    libcapsule
    libcapsule-i686
    libvdpau
    waffle
    vulkan-loader
  ];

  patches = [
    ./linker-paths.patch
  ];

  postPatch = ''
    substituteInPlace steam-runtime-tools/architecture.c \
      --replace "@linker64@" $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
      --replace "@linker32@" $(cat ${stdenv.cc}/nix-support/dynamic-linker-m32)
  '';

  mesonFlags = [
    "-Dmultiarch_tuple=${hostPrefix}"
  ];

  postFixup = ''
    libexecdir=$out/libexec/steam-runtime-tools-0
    ln -s ${waffle}/bin/wflinfo $libexecdir/${hostPrefix}-wflinfo
    ln -s ${vulkan-tools}/bin/vulkaninfo $libexecdir/${hostPrefix}-vulkaninfo

    ${lib.optionalString (steam-runtime-tools-i686 != null) ''
    ln -s ${steam-runtime-tools-i686}/libexec/steam-runtime-tools-0/${steam-runtime-tools-i686.hostPrefix}-* $libexecdir/
    ''}
  '';

  passthru = {
    inherit hostPrefix;
  };

  meta = with lib; {
    description = "Steam Runtime integration for the Steam client";
    homepage = "https://gitlab.steamos.cloud/steamrt/steam-runtime-tools";
    platforms = [ "x86_64-linux" ];
    license = with licenses; [ mit lgpl21Plus asl20 zlib ];
    maintainers = with maintainers; [ tadfisher ];
  };
}

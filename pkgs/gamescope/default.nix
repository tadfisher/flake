{ lib
, stdenv
, callPackage
, src
, glslang
, makeWrapper
, meson
, ninja
, pixman
, pkg-config
, stb
, vulkan-headers
, wayland-protocols
, libcap
, libdrm
, libinput
, libliftoff
, libxkbcommon
, pipewire
, SDL2
, vulkan-loader
, wayland
, wlroots
, xorg
, xwayland
}:

stdenv.mkDerivation rec {
  name = "gamescope-unstable";

  inherit src;

  patches = [ ./meson.patch ];

  nativeBuildInputs = [
    glslang
    makeWrapper
    meson
    ninja
    pixman
    pkg-config
    stb
    vulkan-headers
    wayland-protocols
  ];

  buildInputs = [
    libcap
    libdrm
    libinput
    libliftoff
    libxkbcommon
    SDL2
    pipewire
    vulkan-loader
    wayland
    wlroots
    xwayland
  ] ++ (with xorg; [
    libX11
    libXdamage
    libXcomposite
    libXrender
    libXext
    libXfixes
    libXi
    libXres
    libXxf86vm
    libXtst
  ]);

  dontUseCmakeConfigure = true;

  mesonFlags = [
    "--force-fallback-for="
  ];

  postPatch = ''
    substituteInPlace meson.build --subst-var-by stb ${stb}
  '';

  postInstall = ''
    wrapProgram $out/bin/gamescope --set WLR_XWAYLAND "${xwayland}/bin/Xwayland"
  '';

  meta = with lib; {
    description = "SteamOS session compositing window manager";
    homepage = "https://github.com/Plagman/gamescope";
    license = licenses.bsd2;
    platforms = platforms.linux;
    maintainers = [ maintainers.tadfisher ];
  };
}

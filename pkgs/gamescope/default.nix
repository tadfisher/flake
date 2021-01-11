{ lib
, stdenv
, fetchFromGitHub
, cmake
, glslang
, meson
, ninja
, pixman
, pkgconfig
, vulkan-headers
, wayland-protocols
, libcap
, libdrm
, libinput
, libxkbcommon
, SDL2
, vulkan-loader
, wayland
, wlroots
, xorg
, xwayland
}:

stdenv.mkDerivation rec {
  pname = "gamescope";
  version = "3.7.1";

  src = fetchFromGitHub {
    owner = "Plagman";
    repo = pname;
    rev = version;
    fetchSubmodules = true;
    sha256 = "sha256-MKX7hdRUn9WG7IfUw7ZFj7EXSM6CpIXiKvUPcrDMeVA=";
  };

  nativeBuildInputs = [
    cmake
    glslang
    meson
    ninja
    pixman
    pkgconfig
    vulkan-headers
    wayland-protocols
  ];

  buildInputs = [
    libcap
    libdrm
    libinput
    libxkbcommon
    SDL2
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
    libXxf86vm
    libXtst
  ]);

  dontUseCmakeConfigure = true;

  meta = with lib; {
    description = "SteamOS session compositing window manager";
    homepage = "https://github.com/Plagman/gamescope";
    license = licenses.bsd2;
    platforms = platforms.linux;
    maintainers = [ maintainers.tadfisher ];
  };
}

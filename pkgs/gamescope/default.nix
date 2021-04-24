{ lib
, stdenv
, fetchFromGitHub
, glslang
, makeWrapper
, meson
, ninja
, pixman
, pkg-config
, vulkan-headers
, wayland-protocols
, libcap
, libdrm
, libinput
, libliftoff
, libxkbcommon
, SDL2
, vulkan-loader
, wayland
, wlroots
, xorg
, xwayland
}:
let
  source = builtins.fromJSON (builtins.readFile ./source.json);
  version = builtins.readFile ./version;

  wlroots-git = wlroots.overrideAttrs (attrs: rec {
    pname = "wlroots";
    version = "0.13.0";

    src = fetchFromGitHub {
      owner = "swaywm";
      repo = "wlroots";
      rev = version;
      sha256 = "sha256-/u2gx9oNG0T6TW/y3y7hhKZ8QXZog58VCs+Xq+2C9AY=";
    };
  });

in
stdenv.mkDerivation rec {
  pname = "gamescope-unstable";
  inherit version;

  src = fetchFromGitHub source;

  postUnpack = ''
    rm -rf $sourceRoot/subprojects
  '';

  nativeBuildInputs = [
    glslang
    makeWrapper
    meson
    ninja
    pixman
    pkg-config
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
    vulkan-loader
    wayland
    wlroots-git
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

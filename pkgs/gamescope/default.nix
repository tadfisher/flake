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
    pname = "wlroots-unstable";
    version = "2020-12-15";

    src = fetchFromGitHub {
      owner = "swaywm";
      repo = "wlroots";
      rev = "da2a2169344ef2dbe0dc31fd013caf30880d6aff";
      sha256 = "sha256-aRl4Ljt1KoTMPjlK3aFRRXjora7sHFhzn/7gdXMb/EM=";
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

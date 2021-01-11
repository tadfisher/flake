{ lib
, stdenv
, fetchFromGitHub
, SDL
, SDL_image
, autoreconfHook
, bluez
, coreutils
, gnugrep
, libGL
, libX11
, libXcomposite
, libXdamage
, libXext
, libXfixes
, libXpm
, libXrender
, libXt
, libXxf86vm
, libudev
, motif
, pkgconfig
, pulseaudioFull
, steam
, steamos-modeswitch-inhibitor
, steamos-modeswitch-inhibitor-i686 ? null
, systemd
, xrandr
, xset
}:

stdenv.mkDerivation rec {
  pname = "steamos-compositor-plus";
  version = "1.5.2";

  src = fetchFromGitHub {
    owner = "gamer-os";
    repo = pname;
    rev = version;
    sha256 = "0pw3llvlynqzs6dzpl9ciidyfk3dncpjbvmkxgjsd7d9vy6626xi";
  };

  nativeBuildInputs = [
    autoreconfHook
    pkgconfig
  ];

  buildInputs = [
    bluez
    coreutils
    gnugrep
    libGL
    libX11
    libXcomposite
    libXdamage
    libXext
    libXpm
    libXrender
    libXt
    libXxf86vm
    libudev
    motif
    SDL
    SDL_image
    steam
    steamos-modeswitch-inhibitor
    systemd
    pulseaudioFull
    xrandr
    xset
  ] ++ lib.optional (steamos-modeswitch-inhibitor-i686 != null) [
    steamos-modeswitch-inhibitor-i686
  ];

  patches = [ ./steamos-session.patch ];

  dontAddPrefix = true;

  preConfigure = ''
    configureFlags="--prefix=$out $configureFlags"
  '';

  postInstall = ''
    cp -r usr/bin/* $out/bin
    cp -r usr/share/* $out/share

    chmod +x $out/bin/*
  '';

  modeswitchLibs = lib.makeLibraryPath (
    [ steamos-modeswitch-inhibitor ]
    ++ lib.optional (steamos-modeswitch-inhibitor-i686 != null) steamos-modeswitch-inhibitor-i686
  );

  postFixup = ''
    bluez=${bluez} \
    pulseaudio=${pulseaudioFull} \
    steam=${steam} \
    systemd=${systemd} \
    xset=${xset} \
    substituteAllInPlace $out/bin/steamos-session

    substituteInPlace $out/share/steamos-compositor-plus/bin/set_hd_mode.sh \
      --replace date ${coreutils}/bin/date \
      --replace xrandr ${xrandr}/bin/xrandr \
      --replace cut ${coreutils}/bin/cut \
      --replace tr ${coreutils}/bin/tr \
      --replace grep ${gnugrep}/bin/grep

    substituteInPlace $out/share/xsessions/steamos.desktop \
      --replace steamos-session $out/bin/steamos-session
  '';

  passthru = {
    providedSessions = [ "steamos" ];
  };

  meta = with lib; {
    description = "SteamOS compositing and window manager (gamer-os fork)";
    homepage = "https://github.com/gamer-os/steamos-compositor-plus";
    maintainers = with maintainers; [ tadfisher ];
    license = with licenses; [ bsd2 ];
  };
}

{ lib
, stdenv
, fetchurl
, autoreconfHook
, pkgconfig
, libX11
, libXrandr
, libXrender
, libXxf86vm
}:

stdenv.mkDerivation rec {
  name = "steamos-modeswitch-inhibitor-${version}";
  version = "1.10";

  src = fetchurl {
    url = "https://repo.steampowered.com/steamos/pool/main/s/steamos-modeswitch-inhibitor/steamos-modeswitch-inhibitor_${version}.tar.xz";
    sha256 = "1lskfb4l87s3naz2gmc22q0xzvlhblywf5z8lsiqnkrrxnpbbwj7";
  };

  nativeBuildInputs = [ autoreconfHook pkgconfig ];

  buildInputs = [ libX11 libXrandr libXrender libXxf86vm ];

  meta = with lib; {
    description = "SteamOS mode switch inhibitor";
    longDescription = ''
      Shared library which fakes any mode switch attempts to prevent full-screen
      apps from changing resolution.
    '';
    homepage = http://repo.steamstatic.com/steamos/pool/main/s/steamos-modeswitch-inhibitor/;
    license = licenses.bsd2;
    maintainers = with maintainers; [ tadfisher ];
  };
}

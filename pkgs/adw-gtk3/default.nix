{ stdenv
, lib
, src
, meson
, ninja
, sassc
}:

stdenv.mkDerivation {
  pname = "adw-gtk3";
  version = "unstable";

  inherit src;

  nativeBuildInputs = [ meson ninja sassc ];

  meta = with lib; {
    description = " The theme from libadwaita ported to GTK 3";
    homepage = "https://github.com/lassekongo83/adw-gtk3";
    license = licenses.lgpl21Plus;
    maintainers = with maintainers; [ tadfisher ];
  };
}

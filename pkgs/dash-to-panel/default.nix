{ lib
, stdenv
, glib
, gettext
, src
}:

stdenv.mkDerivation rec {
  pname = "gnome-shell-dash-to-panel";
  version = "42";

  inherit src;

  buildInputs = [
    glib
    gettext
  ];

  makeFlags = [ "INSTALLBASE=$(out)/share/gnome-shell/extensions" ];

  uuid = "dash-to-panel@jderose9.github.com";

  meta = with lib; {
    description = "An icon taskbar for Gnome Shell";
    license = licenses.gpl2;
    maintainers = with maintainers; [ mounium ];
    homepage = "https://github.com/jderose9/dash-to-panel";
  };
}

{ lib
, stdenv
, glib
, gettext
, src
}:

stdenv.mkDerivation rec {
  pname = "gnome-shell-dash-to-panel";
  version = "46";

  inherit src;

  buildInputs = [
    glib
    gettext
  ];

  makeFlags = [ "INSTALLBASE=$(out)/share/gnome-shell/extensions" ];

  passthru.extensionUuid = "dash-to-panel@jderose9.github.com";

  meta = with lib; {
    description = "An icon taskbar for Gnome Shell";
    license = licenses.gpl2Plus;
    maintainers = with maintainers; [ tadfisher ];
    homepage = "https://github.com/jderose9/dash-to-panel";
  };
}
